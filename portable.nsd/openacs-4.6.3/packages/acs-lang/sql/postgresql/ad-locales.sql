--
-- packages/language/sql/language-create.sql
--
-- @author Jeff Davis (davis@xarg.net)
-- @creation-date 2000-09-10
-- @cvs-id $Id: ad-locales.sql,v 1.2 2002/07/10 11:57:19 jeffd Exp $
--

-- ****************************************************************************
-- * The lang_messages table holds the message catalog.
-- * It is populated by ad_lang_message_register.
-- * The registered_p flag denotes that a message exists in a file
-- * that gets loaded on server startup, and hence should not get updated.
-- ****************************************************************************

begin;

create table ad_locales (
  locale		varchar(30)
                        constraint ad_locale_abbrev_pk
                        primary key,
  language		char(2) constraint ad_language_name_nil
			not null,
  country		char(2) constraint ad_country_name_nil
			not null,
  variant		varchar(30),
  label			varchar(200)
                        constraint ad_locale_name_nil
			not null
                        constraint ad_locale_name_unq
                        unique,
  nls_language		varchar(30)
                        constraint ad_locale_nls_lang_nil
			not null,
  nls_territory		varchar(30),
  nls_charset		varchar(30),
  mime_charset		varchar(30),
  -- is this the default locale for its language
  default_p             boolean default 'f'
);

comment on table ad_locales is '
  An OpenACS locale is identified by a language and country.
  Locale definitions in Oracle consist of a language, and optionally
  territory and character set.  (Languages are associated with default
  territories and character sets when not defined).  The formats
  for numbers, currency, dates, etc. are determined by the territory.
  language is two letter abbrev is ISO 639 language code
  country is two letter abbrev is ISO 3166 country code
  mime_charset is IANA charset name
  nls_charset is  Oracle charset name
';

insert into ad_locales (
  locale, label, language, country,
  nls_language, nls_territory, nls_charset, mime_charset, default_p
) values (
  'en_US', 'American', 'en', 'US',
  'AMERICAN', 'AMERICA', 'WE8ISO8859P1', 'ISO-8859-1', 't'
);


insert into ad_locales (
  locale, label, language, country,
  nls_language, nls_territory, nls_charset, mime_charset, default_p
) values (
  'de_DE', 'German', 'de', 'DE',
  'GERMAN', 'GERMANY', 'WE8ISO8859P1', 'ISO-8859-1', 't'
);


insert into ad_locales (
  locale, label, language, country,
  nls_language, nls_territory, nls_charset, mime_charset, default_p
) values (
  'es_ES', 'Spain', 'es', 'ES',
  'SPANISH', 'SPAIN', 'WE8ISO8859P1', 'ISO-8859-1', 't'
);


insert into ad_locales (
  locale, label, language, country,
  nls_language, nls_territory, nls_charset, mime_charset, default_p
) values (
  'fr_FR', 'French', 'fr', 'FR',
  'FRENCH', 'France', 'WE8ISO8859P1', 'ISO-8859-1', 't'
);


insert into ad_locales (
  locale, label, language, country, 
  nls_language, nls_territory, nls_charset, mime_charset, default_p
) values (
  'ja_JP', 'Japanese', 'ja', 'JP',
  'JAPANESE', 'JAPAN', 'JA16SJIS', 'Shift_JIS', 't'
);

end;
