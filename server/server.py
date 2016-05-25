#!/usr/bin/env python

from flask import Flask,jsonify,render_template
import psycopg2
import uuid
import json
import random

db = psycopg2.connect(database='snl')
app = Flask(__name__)

snakes_ladders = {6: 17,
         14: 3,
         20: 15,
         24: 26,
         30: 44,
         39: 33,
         49: 62,
         66: 53,
         69: 58,
         79: 67,
         82: 86,
         84: 71,
         88: 36,
}

gamestates=['pending','ongoing','ended']
playerstates=['playing','finished']
dicestates=[1,2,3,4,5,6]

def gg(gid):
    cur = db.cursor()
    cur.execute("select * from games where id=%(gid)s",{'gid':gid})
    rows = cur.fetchall()
    rt = rows[0][1]
    return rt
def sg(gid,state):
    cur = db.cursor()
    cur.execute("update games set state=%(state)s where id=%(gid)s",{'state':json.dumps(state),'gid':gid})
    cur.execute("commit")

def vg(state):
    "validate the game state"
    assert state['status'] in gamestates,"unknown game state %s"%state['status'];
    if state['turnCount'] and state['status']=='pending': raise Exception('bad state, game begun')
    for p in state['players']:
        assert p['state'] in playerstates
        if p['turnsTaken'] and state['status']=='pending': raise Exception('bad state, game has begun')
    if state['whoseTurn']:
        p = [p for p in state['players'] if p['id']==state['whoseTurn']][0]
        assert p['state']!='ended'
    if len(state['players']):
        playing = filter(lambda lp: lp['state']=='playing',state['players'])
        if not len(playing) and state['status']!='ended': raise Exception('bad state, game has ended')
    elif state['status']!='pending': raise Exception('how could have a game begun without players')

@app.route("/")
def index():
    return render_template('index.html')
@app.route("/game/new")
def newgame():
    gid = str(uuid.uuid1())
    cur = db.cursor()
    s = {'players':[],
         'status':'pending',
         'lastRoll':None,
         'seed':42,
         'whoseTurn':None,
         'turnCount':0,
         'gameOver':False,
         'id':gid,
    }
    ins = {'gid':gid,'state':json.dumps(s)}
    cur.execute("insert into games (id,state) values(%(gid)s,%(state)s)",ins)
    cur.execute("commit")
    return jsonify({'gid':gid,'state':s})
@app.route("/test")
def test():
    return str(random.random())
@app.route("/game/<gid>/state")
def gamestate(gid):
    rt = gg(gid)
    vg(rt)
    return jsonify(rt)

@app.route('/game/<gid>/make_turn')
def make_turn(gid):
    s = gg(gid)
    vg(s)

    #take player
    playing = filter(lambda lp: lp['state']=='playing',s['players'])
    #early exit if no one is playing
    if not len(playing): return jsonify(s)
    
    p = [p for p in s['players'] if p['id']==s['whoseTurn']][0]

    #roll the dice for current player
    roll = random.choice(dicestates)
    pos = p['position']+roll
    if pos in snakes_ladders: pos=snakes_ladders[pos]
    if pos>=99: state='finished'
    else: state=p['state']
    p['state']=state ; p['position']=pos
    p['turnsTaken']+=1
    s['players'] = map(lambda lp: lp['id']==p['id'] and p or lp,s['players'])
    s['status']='ongoing' #mark game as ongoing
    
    #determine next available player
    if not len(playing):
        s['status']='ended'
        s['whoseTurn']=None
    else:
        i = playing.index(p)
        if len(playing)-1>=i+1:
            np = playing[i+1]

        else:
            np = playing[0]
            s['turnCount']+=1
        playing = filter(lambda lp: lp['state']=='playing',s['players'])
        if len(playing):
            s['whoseTurn']=np['id']
        else:
            s['whoseTurn']=None
            s['status']='ended'
    sg(gid,s)
    return jsonify(s)
@app.route('/game/<gid>/player/new')
def addplayer(gid):
    s = gg(gid)
    assert s['status']=='pending'
    pid = str(uuid.uuid1())
    names = [p['name'] for p in s['players']]
    if len(names):
        mx = max(names)
        if len(mx)==1:
            aname = chr(ord(mx)+1)
        else:
            aname = mx+'*'
    else:
        aname = 'a'
    assert aname not in names,"name %s already exists"%aname
    p = {'id':pid,
         'name':aname,
         'state':'playing',
         'position':0,
         'turnsTaken':0}
    s['players'].append(p)
    if not s['whoseTurn']: s['whoseTurn']=pid
    vg(s)
    sg(gid,s)
    return jsonify({'gid':gid,'state':s})

if __name__ == "__main__":
    app.run(debug=True)
            
