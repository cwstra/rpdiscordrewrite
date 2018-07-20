--
-- PostgreSQL database dump
--

-- Dumped from database version 10.4 (Ubuntu 10.4-0ubuntu0.18.04)
-- Dumped by pg_dump version 10.4 (Ubuntu 10.4-0ubuntu0.18.04)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


--
-- Name: hstore; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS hstore WITH SCHEMA public;


--
-- Name: EXTENSION hstore; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION hstore IS 'data type for storing sets of (key, value) pairs';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: channels; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.channels (
    server_id bigint NOT NULL,
    channel_id bigint NOT NULL,
    codex character varying(10),
    charsigns text[],
    charseps text[],
    prefixes text[],
    inline boolean
);


ALTER TABLE public.channels OWNER TO cwstra;

--
-- Name: characters; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.characters (
    server_id bigint,
    member_id bigint,
    character_name text,
    attributes public.hstore
);


ALTER TABLE public.characters OWNER TO cwstra;

--
-- Name: codex_list; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.codex_list (
    id character varying(10) NOT NULL,
    display_name text
);


ALTER TABLE public.codex_list OWNER TO cwstra;

--
-- Name: member_pool_roll_stats; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.member_pool_roll_stats (
    server_id bigint,
    member_id bigint,
    pool text,
    stats public.hstore
);


ALTER TABLE public.member_pool_roll_stats OWNER TO cwstra;

--
-- Name: member_roll_stats; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.member_roll_stats (
    server_id bigint,
    member_id bigint,
    stats text[]
);


ALTER TABLE public.member_roll_stats OWNER TO cwstra;

--
-- Name: server_pool_roll_stats; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.server_pool_roll_stats (
    server_id bigint,
    pool text,
    stats public.hstore
);


ALTER TABLE public.server_pool_roll_stats OWNER TO cwstra;

--
-- Name: server_roll_stats; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.server_roll_stats (
    server_id bigint,
    members bigint[],
    stats text[]
);


ALTER TABLE public.server_roll_stats OWNER TO cwstra;

--
-- Name: servers; Type: TABLE; Schema: public; Owner: cwstra
--

CREATE TABLE public.servers (
    id bigint NOT NULL,
    codex character varying(10),
    charsigns text[],
    charseps text[],
    prefixes text[],
    inline boolean,
    permissionroles bigint[]
);


ALTER TABLE public.servers OWNER TO cwstra;

--
-- Data for Name: channels; Type: TABLE DATA; Schema: public; Owner: cwstra
--

COPY public.channels (server_id, channel_id, codex, charsigns, charseps, prefixes, inline) FROM stdin;
266024279651516416	466726958466727957	ptu_05	\N	\N	\N	\N
466825566566481930	466825823203360778	ptu_05	\N	\N	\N	\N
\.


--
-- Data for Name: characters; Type: TABLE DATA; Schema: public; Owner: cwstra
--

COPY public.characters (server_id, member_id, character_name, attributes) FROM stdin;
266024279651516416	125739721149251591	Jim the Test Muffin	"Flavor"=>"Blueberry", "Strength"=>"1d6", "Alignment"=>"LG"
266024279651516416	125739721149251591	Jeff the Redeemed Cupcake	"Alignment"=>"LG"
409109629352804354	125739721149251591	Tags	"Damage Equation"=>"```1. Find initial Damage Base\n2. Apply Five/Double-Strike\n3. Add Damage Base modifiers (ex: STAB) for final\nDamage Base\n4. Modify damage roll for Critical Hit if applicable\n5. Roll damage or use set damage\n6. Add relevant attack stat and other bonuses\n7. Subtract relevant defense stat and damage reduction 8. Apply weakness and resistance multipliers.\n9. Subtract final damage from target’s Hit Points and\ncheck for Injuries or KO.```"
409109629352804354	125739721149251591	DB	"1"=>"1d6+1", "2"=>"1d6+3", "3"=>"1d6+5", "4"=>"1d8+6", "5"=>"1d8+8", "6"=>"2d6+8", "7"=>"2d6+10", "8"=>"2d8+10", "9"=>"2d10+10", "10"=>"3d8+10", "11"=>"3d10+10", "12"=>"3d12+10", "13"=>"4d10+10", "14"=>"4d10+15", "15"=>"4d10+20", "16"=>"5d10+20", "17"=>"5d12+25", "18"=>"6d12+25", "19"=>"6d12+30", "20"=>"6d12+35", "21"=>"6d12+40", "22"=>"6d12+45", "23"=>"6d12+50", "24"=>"6d12+55", "25"=>"6d12+60", "26"=>"7d12+65", "27"=>"8d12+70", "28"=>"8d12+80"
409109629352804354	246487479803314178	infodump	"taste"=>"```Nature & Flavor Preference: Pokémon prefer a specific type of flavor based on their Nature. Each Stat correlates\nto a flavor; HP with Salty, Attack with Spicy, Defense with Sour, Special Attack with Dry, Special Defense with\nBitter, and Speed with Sweet. Pokémon like the flavor associated with the Stat raised by their nature, and dislike\nthe flavor associated with the stat lowered by their nature. Pokémon with neutral natures do not have any flavor\npreferences.```"
409109629352804354	125739721149251591	Jim the Test Muffin	"img"=>"https://etyman.files.wordpress.com/2011/04/muffin1.jpg"
409109629352804354	225757650065817601	Nature Chart	"img"=>"https://cdn.discordapp.com/attachments/426472730829914114/468125413038882826/Nature_Chart.PNG"
\.


--
-- Data for Name: codex_list; Type: TABLE DATA; Schema: public; Owner: cwstra
--

COPY public.codex_list (id, display_name) FROM stdin;
ptu_pt	PTU 1.05 (With Playtest Packets)
ptu_05	PTU 1.05
ptu_al	PTU 1.05 (With Playtest + Alola)
\.


--
-- Data for Name: member_pool_roll_stats; Type: TABLE DATA; Schema: public; Owner: cwstra
--

COPY public.member_pool_roll_stats (server_id, member_id, pool, stats) FROM stdin;
266024279651516416	125739721149251591	1d6	"1"=>"1", "5"=>"1"
408827715593043968	103836378697048064	3d6	"11"=>"2"
408827715593043968	203156323888529408	3d6	"11"=>"2"
336642139381301249	125739721149251591	16d6	"57"=>"1"
336642139381301249	125739721149251591	5d6	"16"=>"1"
336642139381301249	125739721149251591	207d6	"771"=>"1"
336642139381301249	125739721149251591	57d6	"207"=>"1"
336642139381301249	125739721149251591	1d20	"16"=>"1"
336642139381301249	125739721149251591	1d10	"5"=>"1"
336642139381301249	125739721149251591	10d6	"35"=>"1"
336642139381301249	125739721149251591	1d6	"1"=>"1", "2"=>"1", "3"=>"1", "5"=>"1"
409109629352804354	225757650065817601	3d8	"19"=>"1"
409109629352804354	336706679737548801	3d8	"12"=>"1"
408827715593043968	103836378697048064	6d6	"22"=>"2", "25"=>"1", "26"=>"1"
409109629352804354	336706679737548801	3d6	"12"=>"1"
408827715593043968	203156323888529408	6d6	"22"=>"2", "25"=>"1", "26"=>"1"
409109629352804354	125739721149251591	5d6	"12"=>"1", "13"=>"1", "14"=>"1", "15"=>"1", "17"=>"1", "19"=>"1", "20"=>"2"
408827715593043968	103885660376559616	6d6	"22"=>"2", "25"=>"1", "26"=>"1"
409109629352804354	246487479803314178	4d6	"17"=>"1"
443553586920685588	103836378697048064	2d6	"8"=>"1"
409109629352804354	125739721149251591	4d6	"7"=>"1", "12"=>"1", "13"=>"2", "19"=>"2"
443553586920685588	103836378697048064	2d8	"13"=>"1"
343877986845458432	125739721149251591	1d6	"3"=>"1"
343877986845458432	309277025485717505	4d6	"13"=>"1"
343877986845458432	125739721149251591	10d6	"37"=>"1"
369197451804016642	125739721149251591	11d20	"131"=>"1"
369197451804016642	125739721149251591	1d20	"11"=>"1", "16"=>"1"
369197451804016642	125739721149251591	14927d20	"156291"=>"1"
369197451804016642	125739721149251591	131d20	"1443"=>"1"
369197451804016642	125739721149251591	1443d20	"14927"=>"1"
369197451804016642	125739721149251591	2d100	"106"=>"1"
369197451804016642	338022355861372938	2d8675309	"9159495"=>"1"
266024279651516416	125739721149251591	82d20	"814"=>"1"
443553586920685588	103836378697048064	4d6	"17"=>"1"
266024279651516416	125739721149251591	8692d20	"90452"=>"1"
266024279651516416	125739721149251591	10d20	"82"=>"1"
266024279651516416	125739721149251591	814d20	"8692"=>"1"
266024279651516416	125739721149251591	91d20	"1014"=>"1"
266024279651516416	125739721149251591	1014d20	"10612"=>"1"
443553586920685588	103836378697048064	4d8	"18"=>"1"
266024279651516416	125739721149251591	10612d20	"110852"=>"1"
266024279651516416	125739721149251591	2d20	"21"=>"1"
266024279651516416	125739721149251591	262d20	"2767"=>"1"
443553586920685588	103836378697048064	3d10	"15"=>"1"
266024279651516416	125739721149251591	2767d20	"28882"=>"1"
266024279651516416	125739721149251591	21d20	"262"=>"1"
443553586920685588	203156323888529408	4d10	"17"=>"1"
266024279651516416	125739721149251591	20d20	"198"=>"1"
266024279651516416	125739721149251591	81d20	"862"=>"1"
266024279651516416	125739721149251591	8964d20	"94778"=>"1"
266024279651516416	125739721149251591	8d20	"81"=>"1", "91"=>"1"
266024279651516416	125739721149251591	862d20	"8964"=>"1"
266024279651516416	125739721149251591	7d20	"74"=>"1"
266024279651516416	125739721149251591	779d20	"8150"=>"1"
266024279651516416	125739721149251591	8150d20	"85426"=>"1"
266024279651516416	125739721149251591	1d20	"1"=>"4", "2"=>"1", "7"=>"1", "8"=>"2", "10"=>"1", "20"=>"1"
266024279651516416	125739721149251591	74d20	"779"=>"1"
409109629352804354	125739721149251591	2d6	"6"=>"1"
409109629352804354	125739721149251591	1d6	"1"=>"1", "2"=>"2", "3"=>"1", "6"=>"2"
384873331784024065	140212334063910912	1d6	"2"=>"1"
384873331784024065	140212334063910912	10d6	"42"=>"1"
384873331784024065	140212334063910912	2d100	"112"=>"1"
384873331784024065	419531066727202817	100d20	"1098"=>"1"
214824499395297281	125739721149251591	1d6	"1"=>"1", "3"=>"1", "4"=>"3"
214824499395297281	125739721149251591	10d6	"37"=>"1"
214824499395297281	125739721149251591	4d6	"10"=>"1"
466825566566481930	125739721149251591	1d6	"6"=>"1"
408827715593043968	103836378697048064	3d12	"23"=>"1"
408827715593043968	103836378697048064	6d12	"36"=>"1"
408827715593043968	103836378697048064	6d10	"29"=>"1", "32"=>"1", "35"=>"1"
408827715593043968	103836378697048064	10d10	"60"=>"1"
408827715593043968	103836378697048064	7d10	"38"=>"1"
408827715593043968	103836378697048064	2d6	"8"=>"1"
408827715593043968	203156323888529408	2d6	"4"=>"1"
408827715593043968	103885660376559616	4d6	"8"=>"1", "10"=>"1", "15"=>"1"
\.


--
-- Data for Name: member_roll_stats; Type: TABLE DATA; Schema: public; Owner: cwstra
--

COPY public.member_roll_stats (server_id, member_id, stats) FROM stdin;
443553586920685588	203156323888529408	{4d10}
336642139381301249	125739721149251591	{1d10,5d6,1d20,16d6,1d6,207d6,10d6,57d6}
409109629352804354	225757650065817601	{3d8}
409109629352804354	336706679737548801	{3d6,3d8}
409109629352804354	246487479803314178	{4d6}
343877986845458432	309277025485717505	{4d6}
343877986845458432	125739721149251591	{10d6,1d6}
369197451804016642	125739721149251591	{14927d20,131d20,1443d20,1d20,2d100,11d20}
369197451804016642	338022355861372938	{2d8675309}
266024279651516416	125739721149251591	{1d6,20d20,1d20,21d20,10612d20,862d20,262d20,7d20,81d20,8964d20,8692d20,814d20,2d20,10d20,8150d20,74d20,82d20,779d20,1014d20,91d20,8d20,2767d20}
409109629352804354	125739721149251591	{1d6,5d6,2d6,4d6}
384873331784024065	140212334063910912	{1d6,2d100,10d6}
384873331784024065	419531066727202817	{100d20}
214824499395297281	125739721149251591	{10d6,4d6,1d6}
466825566566481930	125739721149251591	{1d6}
408827715593043968	203156323888529408	{3d6,2d6,6d6}
408827715593043968	103836378697048064	{6d12,3d6,10d10,6d6,3d12,2d6,7d10,6d10}
408827715593043968	103885660376559616	{4d6,6d6}
443553586920685588	103836378697048064	{4d8,2d8,2d6,4d6,3d10}
\.


--
-- Data for Name: server_pool_roll_stats; Type: TABLE DATA; Schema: public; Owner: cwstra
--

COPY public.server_pool_roll_stats (server_id, pool, stats) FROM stdin;
266024279651516416	1d6	"1"=>"1", "5"=>"1"
336642139381301249	16d6	"57"=>"1"
336642139381301249	5d6	"16"=>"1"
336642139381301249	207d6	"771"=>"1"
336642139381301249	57d6	"207"=>"1"
336642139381301249	1d20	"16"=>"1"
336642139381301249	1d10	"5"=>"1"
336642139381301249	10d6	"35"=>"1"
336642139381301249	1d6	"1"=>"1", "2"=>"1", "3"=>"1", "5"=>"1"
409109629352804354	3d8	"12"=>"1", "19"=>"1"
409109629352804354	3d6	"12"=>"1"
409109629352804354	5d6	"12"=>"1", "13"=>"1", "14"=>"1", "15"=>"1", "17"=>"1", "19"=>"1", "20"=>"2"
409109629352804354	4d6	"7"=>"1", "12"=>"1", "13"=>"2", "17"=>"1", "19"=>"2"
343877986845458432	1d6	"3"=>"1"
343877986845458432	4d6	"13"=>"1"
343877986845458432	10d6	"37"=>"1"
369197451804016642	11d20	"131"=>"1"
369197451804016642	1d20	"11"=>"1", "16"=>"1"
369197451804016642	14927d20	"156291"=>"1"
369197451804016642	131d20	"1443"=>"1"
369197451804016642	1443d20	"14927"=>"1"
369197451804016642	2d100	"106"=>"1"
369197451804016642	2d8675309	"9159495"=>"1"
266024279651516416	82d20	"814"=>"1"
266024279651516416	8692d20	"90452"=>"1"
266024279651516416	10d20	"82"=>"1"
266024279651516416	814d20	"8692"=>"1"
266024279651516416	91d20	"1014"=>"1"
266024279651516416	1014d20	"10612"=>"1"
266024279651516416	10612d20	"110852"=>"1"
266024279651516416	2d20	"21"=>"1"
266024279651516416	262d20	"2767"=>"1"
266024279651516416	2767d20	"28882"=>"1"
266024279651516416	21d20	"262"=>"1"
266024279651516416	20d20	"198"=>"1"
266024279651516416	81d20	"862"=>"1"
266024279651516416	8964d20	"94778"=>"1"
266024279651516416	8d20	"81"=>"1", "91"=>"1"
266024279651516416	862d20	"8964"=>"1"
266024279651516416	7d20	"74"=>"1"
266024279651516416	779d20	"8150"=>"1"
266024279651516416	8150d20	"85426"=>"1"
266024279651516416	1d20	"1"=>"4", "2"=>"1", "7"=>"1", "8"=>"2", "10"=>"1", "20"=>"1"
266024279651516416	74d20	"779"=>"1"
409109629352804354	2d6	"6"=>"1"
409109629352804354	1d6	"1"=>"1", "2"=>"2", "3"=>"1", "6"=>"2"
384873331784024065	1d6	"2"=>"1"
384873331784024065	10d6	"42"=>"1"
384873331784024065	2d100	"112"=>"1"
384873331784024065	100d20	"1098"=>"1"
214824499395297281	1d6	"1"=>"1", "3"=>"1", "4"=>"3"
214824499395297281	10d6	"37"=>"1"
214824499395297281	4d6	"10"=>"1"
466825566566481930	1d6	"6"=>"1"
408827715593043968	3d12	"23"=>"1"
408827715593043968	6d12	"36"=>"1"
408827715593043968	6d10	"29"=>"1", "32"=>"1", "35"=>"1"
408827715593043968	10d10	"60"=>"1"
408827715593043968	7d10	"38"=>"1"
408827715593043968	2d6	"4"=>"1", "8"=>"1"
408827715593043968	4d6	"8"=>"1", "10"=>"1", "15"=>"1"
408827715593043968	3d6	"10"=>"1", "11"=>"2"
408827715593043968	6d6	"22"=>"3", "23"=>"1", "24"=>"1", "25"=>"1", "26"=>"1"
443553586920685588	2d6	"8"=>"1"
443553586920685588	2d8	"13"=>"1"
443553586920685588	4d6	"17"=>"1"
443553586920685588	4d8	"18"=>"1"
443553586920685588	3d10	"15"=>"1"
443553586920685588	4d10	"17"=>"1"
\.


--
-- Data for Name: server_roll_stats; Type: TABLE DATA; Schema: public; Owner: cwstra
--

COPY public.server_roll_stats (server_id, members, stats) FROM stdin;
336642139381301249	{125739721149251591}	{1d10,5d6,1d20,16d6,1d6,207d6,10d6,57d6}
408827715593043968	{103836378697048064,103885660376559616,203156323888529408}	{6d12,3d6,10d10,6d6,3d12,2d6,7d10,4d6,6d10}
443553586920685588	{103836378697048064,203156323888529408}	{4d8,2d8,2d6,4d6,4d10,3d10}
343877986845458432	{125739721149251591,309277025485717505}	{10d6,4d6,1d6}
369197451804016642	{125739721149251591,338022355861372938}	{14927d20,131d20,1443d20,2d8675309,1d20,2d100,11d20}
266024279651516416	{125739721149251591}	{1d6,20d20,1d20,21d20,10612d20,862d20,262d20,7d20,81d20,8964d20,8692d20,814d20,2d20,10d20,8150d20,74d20,82d20,779d20,1014d20,91d20,8d20,2767d20}
409109629352804354	{225757650065817601,336706679737548801,125739721149251591,246487479803314178}	{5d6,3d8,1d6,3d6,2d6,4d6}
384873331784024065	{140212334063910912,419531066727202817}	{100d20,2d100,10d6,1d6}
214824499395297281	{125739721149251591}	{10d6,4d6,1d6}
466825566566481930	{125739721149251591}	{1d6}
\.


--
-- Data for Name: servers; Type: TABLE DATA; Schema: public; Owner: cwstra
--

COPY public.servers (id, codex, charsigns, charseps, prefixes, inline, permissionroles) FROM stdin;
343877986845458432	ptu_05	\N	\N	{*,/}	\N	\N
214824499395297281	ptu_al	\N	\N	{/}	t	\N
409109629352804354	ptu_al	\N	\N	{/,<@266020506224820224>,<@266020506224820224>}	t	\N
266024279651516416	ptu_pt	{&,$}	{:}	{/}	t	\N
464115155458850816	ptu_al	\N	\N	{/,<@266020506224820224>,<@266020506224820224>}	\N	\N
384873331784024065	ptu_al	\N	\N	{/,<@266020506224820224>,<@266020506224820224>}	\N	\N
468014426134413313	ptu_al	\N	\N	{!,<@266020506224820224>,<@266020506224820224>}	\N	\N
466825566566481930	ptu_al	\N	\N	{/,<@266020506224820224>,<@266020506224820224>}	\N	\N
408827715593043968	ptu_al	\N	\N	{!,<@266020506224820224>,<@266020506224820224>}	\N	\N
195663848701689866	ptu_al	\N	\N	{!,<@266020506224820224>,<@266020506224820224>}	\N	\N
443553586920685588	ptu_al	\N	\N	{/,!,<@266020506224820224>,<@266020506224820224>}	\N	\N
\.


--
-- Name: channels channels_pkey; Type: CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.channels
    ADD CONSTRAINT channels_pkey PRIMARY KEY (server_id, channel_id);


--
-- Name: codex_list codex_list_pkey; Type: CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.codex_list
    ADD CONSTRAINT codex_list_pkey PRIMARY KEY (id);


--
-- Name: servers servers_pkey; Type: CONSTRAINT; Schema: public; Owner: cwstra
--

ALTER TABLE ONLY public.servers
    ADD CONSTRAINT servers_pkey PRIMARY KEY (id);


--
-- Name: TABLE channels; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.channels TO rpbot;


--
-- Name: TABLE characters; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.characters TO rpbot;


--
-- Name: TABLE codex_list; Type: ACL; Schema: public; Owner: cwstra
--

GRANT SELECT ON TABLE public.codex_list TO rpbot;


--
-- Name: TABLE member_pool_roll_stats; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.member_pool_roll_stats TO rpbot;


--
-- Name: TABLE member_roll_stats; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.member_roll_stats TO rpbot;


--
-- Name: TABLE server_pool_roll_stats; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.server_pool_roll_stats TO rpbot;


--
-- Name: TABLE server_roll_stats; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.server_roll_stats TO rpbot;


--
-- Name: TABLE servers; Type: ACL; Schema: public; Owner: cwstra
--

GRANT ALL ON TABLE public.servers TO rpbot;


--
-- PostgreSQL database dump complete
--

