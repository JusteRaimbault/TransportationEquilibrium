
import sqlite3,datetime,numpy,os

dbname = '../../Data/Sytadin/data/sytadin_20180203.sqlite3'

def split_db():
    conn = sqlite3.connect(dbname)
    c = conn.cursor()
    # get all timestamps
    ts = list(map(lambda x : x[0],list(c.execute('SELECT ts FROM data;'))))
    # split in equal size sections
    sortedts = sorted(set(ts))
    #print(sortedts)
    step = 20000
    for k in numpy.arange(0,len(sortedts),step):
        currentts = round(sortedts[k])
        nextts = round(sortedts[min(len(sortedts)-1,k+step)])
        currentdate = datetime.datetime.fromtimestamp(currentts).strftime('%Y%m%d')
        nextdate = datetime.datetime.fromtimestamp(nextts).strftime('%Y%m%d')
        print(currentdate+'-'+nextdate)
        try :
            os.remove('../../Data/Sytadin/data/traffic_'+currentdate+'-'+nextdate+'.sqlite3')
        except OSError:
            pass
        currentconn = sqlite3.connect('../../Data/Sytadin/data/traffic_'+currentdate+'-'+nextdate+'.sqlite3')
        currentconn.cursor().execute('CREATE TABLE data (troncon text, tps_th real, tps real, confiance real, ts real);')
        currentconn.commit()
        currentdata = c.execute('SELECT * FROM data WHERE ts >= '+str(currentts)+' AND ts < '+str(nextts)+';')
        #print(currentdata)
        currentconn.cursor().executemany("INSERT INTO data ('troncon', 'tps_th', 'tps', 'confiance', ts) VALUES (?, ?, ?, ?, ?)", currentdata)
        currentconn.commit()
        currentconn.close()
    c.close()

split_db()
