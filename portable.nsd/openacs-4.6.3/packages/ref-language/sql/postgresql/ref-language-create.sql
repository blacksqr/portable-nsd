-- packages/ref-language/sql/postgresql/language.sql
--
-- @author jon@jongriffin.com
-- @creation-date 2000-11-21
-- @cvs-id $Id: ref-language-create.sql,v 1.1 2001/09/15 00:49:28 donb Exp $
--


-- ISO 639
create table language_codes (
    language_id char(2)
        constraint language_codes_language_id_pk
        primary key,
    name varchar(100)
        constraint language_codes_name_uq
        unique
        constraint language_codes_name_nn
        not null
);

comment on table language_codes is '
    This is data from the ISO 639 standard on language codes.
';

comment on column language_codes.language_id is '
    This is the ISO standard language code
';

comment on column language_codes.name is '
    This is the English version of the language name. 
    I don''t want to get crazy here!
';

-- now register this table with the repository
select acs_reference__new(
    'LANGUAGE_CODES',
    null,
    'ISO 639',
    'http://www.iso.ch',
    now()
);

-- data
begin;
\i ../common/ref-language-data.sql
end;
