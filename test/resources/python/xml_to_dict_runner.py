#!/usr/bin/env python

from os import walk, path
from  xmltodict import parse
from simplejson import load, dumps, loads
from pdb import set_trace
from toolz import thread_first, thread_last, first, last
import psycopg2
from psycopg2.extras import RealDictCursor

def list_files():
    for root, di, files in walk("./"):
        for fi in files:
            if fi.endswith(".xml"):
                xml = slurp(fi)
                yield fi, xml


def load_force_list():
    fl = loads(slurp("force_list.json")                 )
    with psycopg2.connect("host=localhost") as conn, \
        conn.cursor() as cur:
        conn.autocommit = True
        cur.execute("truncate table force_list")
        for x in fl:
            cur.execute("insert into force_list(force_list) values (%s)", [x] )


def load_xml():
    with psycopg2.connect("host=localhost") as conn, \
        conn.cursor() as cur:
        conn.autocommit = True
        cur.execute("truncate table xmltest;")
        for x in list_files():
            cur.execute("insert into xmltest(file_name, xml) values (%s, %s)",
                        x)



load_xml()
with psycopg2.connect("host=localhost") as conn, \
     conn.cursor(cursor_factory=DictCursor) as cur:
    conn.autocommit = True
    cur.execute("truncate table jsontest")
    cur.execute("select * from force_list")
    force_list = [x['force_list'] for x in cur.fetchall()]
    cur.execute("select * from xmltest")
    xmls = cur.fetchall()
    for xml_row in xmls:
        row_id = xml_row['row_id']
        xml = xml_row['xml']
        file_name = xml_row['file_name']
        json = dumps(parse(xml))
        sql = "insert into jsontest(row_id, val, program, file_name) values (%s, %s, %s, %s)"
        cur.execute(sql, [row_id, json, 'py', file_name])
        json_fl = dumps(parse(xml, force_list=force_list))
        cur.execute(sql, [row_id, json_fl, 'py fl', file_name])
    cur.execute("update jsontest set md5_checksum = md5(val::text)::uuid where md5_checksum is null; " )
