package org.kissweb.database;

import org.junit.jupiter.api.Test;
import org.kissweb.database.Connection.ConnectionType;
import org.kissweb.database.SortBuilder.Sort;

import java.util.Arrays;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.*;

class SortBuilderTest {

    private static final String JOIN = "left join lateral (select 1) le on true";

    private static SortBuilder sb() {
        return new SortBuilder()
                .key("name", SortBuilder.text("og.name"))
                .key("rep", SortBuilder.text("p.lname"), SortBuilder.text("p.fname"))
                .key("dt", SortBuilder.zeroBlank("pr.dt"))
                .key("seq", SortBuilder.raw("st.seqno"))
                .key("mail", SortBuilder.raw("le.date_sent"))
                .key("mail2", SortBuilder.raw("le.template"))
                .join(JOIN, "mail", "mail2");
    }

    private static final ConnectionType PG = ConnectionType.PostgreSQL;

    @Test
    void postgresSingleAscDesc() {
        SortBuilder.Result r = sb().build(PG, Arrays.asList(Sort.of("name", true)), null);
        assertEquals("order by lower(nullif(btrim(og.name), '')) asc nulls last", r.orderBy());
        r = sb().build(PG, Arrays.asList(Sort.of("dt", false)), null);
        assertEquals("order by nullif(pr.dt, 0) desc nulls last", r.orderBy());
        r = sb().build(PG, Arrays.asList(Sort.of("seq", true)), null);
        assertEquals("order by st.seqno asc nulls last", r.orderBy());
    }

    @Test
    void multiPartKeyGetsDirectionOnEachPart() {
        SortBuilder.Result r = sb().build(PG, Arrays.asList(Sort.of("rep", false)), null);
        assertEquals("order by lower(nullif(btrim(p.lname), '')) desc nulls last, "
                + "lower(nullif(btrim(p.fname), '')) desc nulls last", r.orderBy());
    }

    @Test
    void twoKeysDefaultIgnoredTiebreakLast() {
        SortBuilder.Result r = sb().build(PG, Arrays.asList(Sort.of("seq", true), Sort.of("dt", false)),
                Sort.of("name", true), "lower(og.name) asc", "pr.id asc");
        assertEquals("order by st.seqno asc nulls last, nullif(pr.dt, 0) desc nulls last, "
                + "lower(og.name) asc, pr.id asc", r.orderBy());
        assertTrue(r.rejectedKeys().isEmpty());
    }

    @Test
    void defaultUsedWhenNoUsableKey() {
        SortBuilder.Result r = sb().build(PG, Arrays.asList(Sort.of(null, true), Sort.of("  ", true), Sort.of("bogus", true)),
                Sort.of("name", true), "pr.id asc");
        assertEquals("order by lower(nullif(btrim(og.name), '')) asc nulls last, pr.id asc", r.orderBy());
        assertEquals(Collections.singletonList("bogus"), r.rejectedKeys());
    }

    @Test
    void secondKeyUsedWhenFirstUnknown() {
        SortBuilder.Result r = sb().build(PG, Arrays.asList(Sort.of("bogus", true), Sort.of("seq", false)), Sort.of("name", true));
        assertEquals("order by st.seqno desc nulls last", r.orderBy());
    }

    @Test
    void blankIgnoredSilentlyUnknownReportedOnceTrimmed() {
        SortBuilder.Result r = sb().build(PG, Arrays.asList(Sort.of("", true), Sort.of(" x;drop ", true), Sort.of("x;drop", false), null), null);
        assertEquals("", r.orderBy());
        assertEquals(Collections.singletonList("x;drop"), r.rejectedKeys());
        assertFalse(r.orderBy().contains("drop"));
    }

    @Test
    void nullInputsAndOnlyTiebreak() {
        assertEquals("order by a asc", sb().build(PG, null, null, "a asc").orderBy());
        assertEquals("", sb().build(PG, null, null).orderBy());
    }

    @Test
    void joinsEmittedOnceOnlyWhenUsed() {
        SortBuilder.Result r = sb().build(PG, Arrays.asList(Sort.of("mail", true), Sort.of("mail2", true)), null);
        assertEquals(Collections.singletonList(JOIN), r.joinList());
        assertEquals(JOIN + " ", r.joins());
        r = sb().build(PG, Arrays.asList(Sort.of("name", true)), null);
        assertEquals("", r.joins());
        assertTrue(r.joinList().isEmpty());
    }

    @Test
    void joinForDefaultKey() {
        SortBuilder.Result r = sb().build(PG, null, Sort.of("mail", false));
        assertEquals(JOIN + " ", r.joins());
        assertEquals("order by le.date_sent desc nulls last", r.orderBy());
    }

    @Test
    void unregisteredDefaultIsProgrammingError() {
        assertThrows(IllegalArgumentException.class, () -> sb().build(PG, null, Sort.of("nope", true)));
        assertThrows(IllegalArgumentException.class, () -> sb().join("x", "nope"));
        assertThrows(IllegalArgumentException.class, () -> new SortBuilder().key(" ", SortBuilder.raw("x")));
    }

    @Test
    void exprLambdaReceivesDialect() {
        SortBuilder s = new SortBuilder().key("t", t -> "case c when 'T' then " + SortBuilder.textSql(t, "n") + " end");
        assertEquals("order by case c when 'T' then lower(nullif(btrim(n), '')) end asc nulls last",
                s.build(PG, Arrays.asList(Sort.of("t", true)), null).orderBy());
    }

    @Test
    void sqliteAndOracleDialects() {
        SortBuilder s = sb();
        assertEquals("order by lower(nullif(trim(og.name), '')) desc nulls last",
                s.build(ConnectionType.SQLite, Arrays.asList(Sort.of("name", false)), null).orderBy());
        assertEquals("order by lower(trim(og.name)) asc nulls last",
                s.build(ConnectionType.Oracle, Arrays.asList(Sort.of("name", true)), null).orderBy());
    }

    @Test
    void mySqlAndSqlServerEmulateNullsLast() {
        assertEquals("order by case when lower(nullif(trim(og.name), '')) is null then 1 else 0 end, "
                + "lower(nullif(trim(og.name), '')) desc, pr.id asc",
                sb().build(ConnectionType.MySQL, Arrays.asList(Sort.of("name", false)), null, "pr.id asc").orderBy());
        assertEquals("order by case when lower(nullif(ltrim(rtrim(p.lname)), '')) is null then 1 else 0 end, "
                + "lower(nullif(ltrim(rtrim(p.lname)), '')) asc, "
                + "case when lower(nullif(ltrim(rtrim(p.fname)), '')) is null then 1 else 0 end, "
                + "lower(nullif(ltrim(rtrim(p.fname)), '')) asc",
                sb().build(ConnectionType.MicrosoftServer, Arrays.asList(Sort.of("rep", true)), null).orderBy());
        assertEquals("order by case when nullif(pr.dt, 0) is null then 1 else 0 end, nullif(pr.dt, 0) desc",
                sb().build(ConnectionType.MySQL, Arrays.asList(Sort.of("dt", false)), null).orderBy());
    }
}
