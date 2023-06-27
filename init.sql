
CREATE TABLE users (
    user_id serial NOT NULL PRIMARY KEY,
    user_name character varying(20) NOT NULL UNIQUE,
    user_password character varying(20) NOT NULL,
    user_active character(1) NOT NULL,
    CONSTRAINT users_active_chk CHECK (((user_active = 'Y') OR (user_active = 'N')))
);

INSERT INTO users (user_name, user_password, user_active) VALUES ('kiss', 'password', 'Y');


-- The following is only used for the SQL demo on the "SQL Access" screen

CREATE TABLE phone (
    rec_id serial NOT NULL PRIMARY KEY,
    last_name character varying(20) NOT NULL,
    first_name character varying(20) NOT NULL,
    phone_number character varying(25)
);

CREATE INDEX phone_name_idx ON phone (last_name, first_name);

INSERT INTO phone (first_name, last_name, phone_number) VALUES ('Blake', 'McBride', '615-394-5566');

