#!/usr/bin/env python
from __future__ import division
import random
import time
from collections import defaultdict
import sys
#immutable
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

def draw_winners(win,throws):
    print 'winners: %s'%','.join([w[0]+' (%s throws)'%throws[w[0]] for w in win])

def draw_board(snakes_ladders,players,throws):
    rt='\|'
    row=0
    for i in range(0,10): rt+=str(i)
    rt+='|\n |'
    for i in range(0,10): rt+='-'
    rt+='|\n%s|'%row
    
    for i in range(0,100):
        if i in snakes_ladders:
            if snakes_ladders[i]<i: rt+= '~'
            else: rt+= 'H'
        else:
            players_here = filter(lambda (pk,pv): pv==i,players.items())
            if len(players_here)>1:
                rt+=str(len(players_here))
            elif len(players_here)==1:
                rt+=str(( players_here[0][0]))
            else:
                rt+= ' '
        if (i+1) % 10 == 0 and i>0:
            row+=1
            rt+='|\n'
            if row<10:
                rt+='%s|'%row
    win = winners(players)
    if len(win): 
        draw_winners(win,throws)
    print rt

def foldl(x,y):
    return x+y
def draw_stats(throws,throws_amt,num_players,games,throws_sum):
    if True: #len(winturns) % 1000==0:
        #print throws

        print 'games:',games,\
            'throws_sum',throws_sum,\
            'throws:',len(throws),\
            'total throws:',throws_amt,\
            'avg throws:','%4.2f'%(len(throws) and (reduce(foldl, throws) / len(throws)) or 0),\
            'tpppg:','%4.2f'%(len(throws) and (throws_amt/num_players/len(throws)) or 0),\
            'min throws:',(throws and min(throws) or '--'),\
            'max throws:',(throws and max(throws) or '--')
    
def board_react(snakes_ladders,players):
    for pk,pv in players.items():
        if pv in snakes_ladders:
            players[pk]=snakes_ladders[pv]
    return players
    pass

def winners(players):
    return filter(lambda (pk,pv): pv>=100,
                     players.items())
    
def throw_dice(players,pk):
    r = random.randrange(1,7)
    #print pk,len(players),r
    players[pk]+=r
    return players,r

def game(players):
    throws = defaultdict(int)
    throws_sum=0
    wins=[]
    winturns=[]
    while len(wins)<len(players):
        for pk in players:
            if pk not in [w[0] for w in wins]:
                players,tsum = throw_dice(players,pk)
                throws_sum+=tsum
                throws[pk]+=1
                players = board_react(snakes_ladders,players)
                wins = winners(players)
                #draw_board(snakes_ladders,players,throws) ; time.sleep(0.1)
    return throws,wins,throws_sum


if __name__=='__main__':
    for num_players in range(1,5):
        throws=[]
        throws_amt=0
        games=0
        throws_sum=0
        print 'num_players=',num_players
        
        #draw_stats(throws,throws_amt,num_players,games,throws_sum)        
        for i in range(100000): #while True:
            players = dict([(chr(ord('a')+i),0) for i in range(0,num_players)])

            gthrows,gwins,gthrows_sum = game(players)
            throws_sum+=gthrows_sum
            throws+=gthrows.values()
            throws_amt+=sum(gthrows.values())
            games+=1
        draw_stats(throws,throws_amt,num_players,games,throws_sum)
