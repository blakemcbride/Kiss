package org.kissweb.restServer;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.annotation.WebFilter;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;

/**
 * Adds baseline security response headers to every response, including the
 * Content-Security-Policy.
 * <br><br>
 * CSP is delivered as a response <em>header</em> rather than an
 * <code>&lt;meta&gt;</code> tag for two reasons: the Report-Only form used during
 * rollout is invalid in a meta tag (browsers ignore it), and a header is the
 * single source of truth for the production/Electron deployments (all served by
 * this Tomcat). The XSS-relevant directives therefore live here.
 * <br><br>
 * <b>Rollout:</b> {@link #CSP_REPORT_ONLY} now ships <code>false</code>, so the
 * XSS policy is <em>enforced</em> (sent as <code>Content-Security-Policy</code>) —
 * violations are blocked, not merely reported. To re-validate after changing the
 * policy, set it back to <code>true</code>: the policy is then sent as
 * <code>Content-Security-Policy-Report-Only</code> (violations reported to the
 * console, nothing blocked); exercise every screen (CKEditor, AG-Grid, file upload,
 * binary images, login/logout) and confirm the console is clean before enforcing
 * again. Because the dev static server does not set headers, validate
 * by browsing the app through the Tomcat origin (the application's back-end
 * port, e.g. http://localhost:8001 with the default port block).
 * <br><br>
 * Self-registering via {@code @WebFilter} annotation scanning (no
 * <code>web.xml</code> entry), so it ships entirely inside the application WAR
 * and requires no servlet-container configuration.
 * <br><br>
 * Author: Blake McBride
 */
@WebFilter(filterName = "SecurityHeadersFilter", urlPatterns = "/*", asyncSupported = true)
public class SecurityHeadersFilter implements Filter {

    /**
     * The XSS-relevant Content-Security-Policy.
     * <br><br>
     * <code>style-src 'unsafe-inline'</code> is required by CKEditor / AG-Grid;
     * <code>img-src data: blob:</code> by {@code Server.binaryCall} images.
     * Pages this filter covers are served by the back end itself, so
     * <code>connect-src 'self'</code> suffices (the dev static server sends no
     * headers at all). For a separated production back-end, add that origin to
     * <code>connect-src</code>.
     */
    public static final String CONTENT_SECURITY_POLICY =
            "default-src 'self'; " +
            //  The one allowed inline script is the byte-stable bootstrap kernel in index.html.
            //  If that kernel is ever edited, recompute this hash.
            "script-src 'self' 'sha256-JaGBSNrOPztIc2kSDoiyDCHVqjcvVQSMYv5X2fO3RCE='; " +
            "style-src 'self' 'unsafe-inline'; " +
            "img-src 'self' data: blob:; " +
            "font-src 'self' data:; " +
            "connect-src 'self'; " +
            "object-src 'none'; " +
            "base-uri 'self'; " +
            "form-action 'self'";

    /**
     * <code>true</code> &rArr; send the policy as
     * <code>Content-Security-Policy-Report-Only</code> (log violations, block
     * nothing). <code>false</code> &rArr; enforce it. Flip to <code>false</code>
     * once the browser console is free of CSP violations.
     * <br><br>
     * May be overridden per deployment with <code>CspReportOnly = true</code>
     * in <code>application.ini</code>.
     */
    private static final boolean CSP_REPORT_ONLY = false;

    /**
     * Master switch. Set to <code>false</code> to emit no security headers at
     * all (the filter becomes a pass-through). Useful for A/B isolating whether
     * these headers are responsible for some observed behavior.
     * <br><br>
     * May be overridden per deployment with <code>SecurityHeaders = false</code>
     * in <code>application.ini</code> — for example, an application whose front
     * end predates the byte-stable bootstrap kernel (whose hash the CSP pins)
     * and would otherwise be blocked by the inline-script policy.
     */
    private static final boolean ENABLED = true;

    /**
     * Default constructor.
     */
    public SecurityHeadersFilter() {
    }

    private static boolean iniOverride(String key, boolean compiledDefault) {
        String v = (String) MainServlet.getEnvironment(key);
        if (v == null || v.isEmpty())
            return compiledDefault;
        return !"false".equalsIgnoreCase(v);
    }

    @Override
    public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain)
            throws IOException, ServletException {
        if (iniOverride("SecurityHeaders", ENABLED) && res instanceof HttpServletResponse) {
            final HttpServletResponse response = (HttpServletResponse) res;

            //  Don't let the browser guess content types.
            response.setHeader("X-Content-Type-Options", "nosniff");

            //  Don't leak URLs (which may carry app state) to other origins.
            response.setHeader("Referrer-Policy", "no-referrer");

            //  Clickjacking protection is always enforced (frame-ancestors must be a header, not a meta tag).
            response.addHeader("Content-Security-Policy", "frame-ancestors 'none'");
            response.setHeader("X-Frame-Options", "DENY");

            //  The XSS policy: report-only during rollout, enforcing afterward.
            response.addHeader(iniOverride("CspReportOnly", CSP_REPORT_ONLY)
                               ? "Content-Security-Policy-Report-Only" : "Content-Security-Policy",
                               CONTENT_SECURITY_POLICY);

            //  HSTS only over HTTPS; never on plain-http dev (e.g. http://localhost).
            if (req instanceof HttpServletRequest && ((HttpServletRequest) req).isSecure())
                response.setHeader("Strict-Transport-Security", "max-age=31536000; includeSubDomains");
        }
        chain.doFilter(req, res);
    }
}
