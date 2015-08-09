

MY_TEAM_ID=99
 
import os
os.system('rm rankings.js')
os.system('wget https://davar.icfpcontest.org/rankings.js')
s = open('rankings.js').readline()[len('var data = '):]
d = eval(s)
 
time = d['time']
print(time)
 
maps = d['data']['settings']
 
scores = {}
for m in maps:
    setting = m['setting']
    for scr in m['rankings']:
        if scr['teamId'] == MY_TEAM_ID:
            scores[setting] = (scr['rank'],scr['score'],
                    scr['power_score'],scr['tags'])
            break
 
for s in scores:
    print(s, scores[s])
