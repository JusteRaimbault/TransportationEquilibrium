
# setup sqlite database for sytadin traffic collection
import sqlite3

def setup():
    conn = sqlite3.connect('data/sytadin.sqlite3')
    c = conn.cursor()
    c.execute('CREATE TABLE data (troncon text, tps_th real, tps real, confiance real, ts real);')
    conn.commit()
    conn.close()

setup()
