
CREATE TABLE users (
    user_id serial NOT NULL PRIMARY KEY,
    user_name character varying(20) NOT NULL UNIQUE,
    user_password character varying(20) NOT NULL,
    user_active character(1) NOT NULL,
    CONSTRAINT users_active_chk CHECK (((user_active = 'Y') OR (user_active = 'N')))
);

INSERT INTO users (user_name, user_password, user_active) VALUES ('kiss', 'password', 'Y');


-- The following is only used for the SQL demo on the "SQL Access" screen

CREATE TABLE test (
    key smallint not null primary key,
    data character varying(40)
);

INSERT INTO test (key, data) VALUES (1, 'Record one');
INSERT INTO test (key, data) VALUES (2, 'Record two');
INSERT INTO test (key, data) VALUES (3, 'Record three');
