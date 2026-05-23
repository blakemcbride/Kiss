package org.kissweb.oauth.as;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.IniFile;
import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;

import java.io.IOException;
import java.math.BigInteger;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.interfaces.RSAPrivateCrtKey;
import java.security.interfaces.RSAPublicKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.Base64;

/**
 * Manages the RSA keypair the authorization server uses to sign access
 * tokens.  Generated on first startup (RSA-2048) and persisted to the
 * {@code [keys]} section of {@code oauth.ini} as base64-encoded PKCS#8
 * (private) and X.509 (public) blobs.  Subsequent startups reload from
 * the file, so issued tokens remain verifiable across restarts.
 * <br><br>
 * Currently maintains a single ``current'' key.  Key rotation
 * (generating a new key while keeping the previous one valid for the
 * duration of any outstanding tokens) is a planned extension --- the
 * JWKS endpoint already publishes a {@link JSONArray} so rotation
 * requires adding a {@code previous} key slot without changing the
 * external interface.
 */
public final class KeyManager {

    private static final Logger logger = LogManager.getLogger(KeyManager.class);

    private static final String SECTION = "keys";

    private static volatile KeyManager instance;

    private final String     kid;
    private final KeyPair    keyPair;

    private KeyManager(String kid, KeyPair keyPair) {
        this.kid     = kid;
        this.keyPair = keyPair;
    }

    /**
     * Get (or create) the AS signing key.  On first call after a fresh
     * install, generates a new keypair and persists it.
     *
     * @return the key manager
     */
    public static KeyManager get() {
        KeyManager local = instance;
        if (local == null) {
            synchronized (KeyManager.class) {
                local = instance;
                if (local == null) {
                    local = load();
                    instance = local;
                }
            }
        }
        return local;
    }

    /** Reset the singleton.  Intended for tests. */
    public static synchronized void reset() {
        instance = null;
    }

    private static KeyManager load() {
        final OAuthIniStore store = OAuthIniStore.get();
        final String wantedKid = AuthorizationServerConfig.get().getKeyId();
        synchronized (store) {
            final IniFile ini = store.ini();
            final String storedKid     = ini.get(SECTION, "current_kid");
            final String storedPrivate = ini.get(SECTION, "current_private");
            final String storedPublic  = ini.get(SECTION, "current_public");

            if (storedKid != null && storedPrivate != null && storedPublic != null) {
                try {
                    final KeyPair kp = decodeKeyPair(storedPrivate, storedPublic);
                    logger.info("Loaded OAuth AS signing key kid=" + storedKid);
                    return new KeyManager(storedKid, kp);
                } catch (Exception e) {
                    logger.error("Stored AS signing key is unreadable; generating a new one", e);
                }
            }

            // No usable key on file --- generate and persist a fresh one.
            final KeyPair kp = generate();
            ini.put(SECTION, "current_kid",      wantedKid);
            ini.put(SECTION, "current_private",  Base64.getEncoder().encodeToString(kp.getPrivate().getEncoded()));
            ini.put(SECTION, "current_public",   Base64.getEncoder().encodeToString(kp.getPublic().getEncoded()));
            ini.put(SECTION, "created_at",       String.valueOf(System.currentTimeMillis() / 1000L));
            try {
                store.save();
            } catch (IOException e) {
                throw new IllegalStateException("Unable to save freshly-generated AS signing key", e);
            }
            logger.info("Generated and persisted new OAuth AS signing key kid=" + wantedKid);
            return new KeyManager(wantedKid, kp);
        }
    }

    /**
     * Get the key id ({@code kid}) embedded in JWT headers and the JWKS.
     *
     * @return the kid
     */
    public String getKid() {
        return kid;
    }

    /**
     * Get the private signing key.
     *
     * @return the private key
     */
    public PrivateKey getPrivateKey() {
        return keyPair.getPrivate();
    }

    /**
     * Get the public verification key.
     *
     * @return the public key
     */
    public PublicKey getPublicKey() {
        return keyPair.getPublic();
    }

    /**
     * Build the JWKS document this AS publishes.  Contains one entry:
     * the current public key, marked for signature use ({@code use=sig})
     * with algorithm {@code RS256}.
     *
     * @return a JWKS-shaped JSON object
     */
    public JSONObject buildJwks() {
        final RSAPublicKey pub = (RSAPublicKey) keyPair.getPublic();
        final JSONObject jwk = new JSONObject();
        jwk.put("kty", "RSA");
        jwk.put("use", "sig");
        jwk.put("alg", "RS256");
        jwk.put("kid", kid);
        jwk.put("n", Base64.getUrlEncoder().withoutPadding()
                .encodeToString(toUnsignedBytes(pub.getModulus())));
        jwk.put("e", Base64.getUrlEncoder().withoutPadding()
                .encodeToString(toUnsignedBytes(pub.getPublicExponent())));

        final JSONArray keys = new JSONArray();
        keys.put(jwk);
        final JSONObject doc = new JSONObject();
        doc.put("keys", keys);
        return doc;
    }

    // ------------------------------------------------------------------

    private static KeyPair generate() {
        try {
            final KeyPairGenerator gen = KeyPairGenerator.getInstance("RSA");
            gen.initialize(2048);
            return gen.generateKeyPair();
        } catch (NoSuchAlgorithmException e) {
            // RSA is in every JRE.
            throw new IllegalStateException(e);
        }
    }

    private static KeyPair decodeKeyPair(String privateB64, String publicB64)
            throws NoSuchAlgorithmException, InvalidKeySpecException {
        final byte[] privBytes = Base64.getDecoder().decode(privateB64);
        final byte[] pubBytes  = Base64.getDecoder().decode(publicB64);
        final KeyFactory kf = KeyFactory.getInstance("RSA");
        final PrivateKey privateKey = kf.generatePrivate(new PKCS8EncodedKeySpec(privBytes));
        final PublicKey  publicKey  = kf.generatePublic(new X509EncodedKeySpec(pubBytes));
        // Sanity check that the loaded key is actually RSA.
        if (!(privateKey instanceof RSAPrivateCrtKey))
            throw new InvalidKeySpecException("Stored private key is not RSA");
        return new KeyPair(publicKey, privateKey);
    }

    /** BigInteger.toByteArray() prepends a sign byte; JWK wants the unsigned magnitude. */
    private static byte[] toUnsignedBytes(BigInteger v) {
        final byte[] raw = v.toByteArray();
        if (raw[0] == 0) {
            final byte[] out = new byte[raw.length - 1];
            System.arraycopy(raw, 1, out, 0, out.length);
            return out;
        }
        return raw;
    }
}
