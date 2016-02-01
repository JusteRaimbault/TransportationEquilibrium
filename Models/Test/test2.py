# test for sytadin Scrapping

import requests,time,sqlite3
from lxml import html,etree

def str_norm(s):
    #print(s.text_content().replace(' ',''))
    #print(len(s.text_content().replace(' ','').replace('\n','').replace('\t','')))
    if len(s.text_content().replace(' ','').replace('\n','').replace('\t',''))==0 :
        return("0")
    else :
        res = s.text_content().replace('\'','').replace(' ','').replace('\n','').replace('\t','').replace('mn','').replace('%','').replace('<','').replace('>','')
        if len(res)>5 : return(res)
        t=res.split("h")
        if len(t)>1 :
            if len(t[1])>0 :
                return(str(60*int(t[0])+int(t[1])))
            else:
                return(str(60*int(t[0])))
        else :
            return(res)

result = requests.get('http://www.sytadin.fr/sys/refreshed/temps_de_parcours.jsp.html')
tree = html.fromstring(result.content)

#times = tree.xpath('//div[@id="secteur"]/div[@class="tps_parcours BP"]/div/table/tbody/tr/text()')

#print(tree)

#tbody = tree.get_element_by_id("secteur")[2][2][1][1]

conn = sqlite3.connect('data/sytadin.sqlite3')
c = conn.cursor()

t = time.time()

for div in tree.get_element_by_id("secteur").findall('div'):
    for subdiv in div.findall('div') :
        #tbody = div[2][1][1]
        tbody = subdiv[1][1]
        for e in tbody.findall('tr'):
            rows =  e.findall('td')
            request = 'INSERT INTO data VALUES (\''+str_norm(rows[2])+'\','+str_norm(rows[3])+','+str_norm(rows[4])+','+str_norm(rows[6])+','+str(t)+');'
            #print(request)
            c.execute(request)
        #print(str_norm(rows[2])+' : '+str_norm(rows[3])+' ; '+str_norm(rows[4])+' - '+str_norm(rows[6]))


conn.commit()
conn.close()
