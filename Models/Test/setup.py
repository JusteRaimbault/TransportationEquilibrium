
# setup sqlite database for sytadin traffic collection
import sqlite3

def setup_sytadin():
    conn = sqlite3.connect('data/sytadin.sqlite3')
    c = conn.cursor()
    c.execute('CREATE TABLE data (troncon text, tps_th real, tps real, confiance real, ts real);')
    conn.commit()
    conn.close()

def setup_gmaps():
    conn = sqlite3.connect('data/gmaps.sqlite3')
    c = conn.cursor()
    c.execute('CREATE TABLE data (ts real, id real, tps real);')
    conn.commit()
    conn.close()

setup_gmaps()
