#!/bin/bash
S=snl-dev
I=0
function inc() {
    I=$[$I+1]
    }
tmux new-session -d -s $S
tmux send-keys -t $S:$I "cd server/ && source bin/activate" C-m
tmux send-keys -t $S:$I "./server.py" C-m

inc
tmux new-window -t $S:$I -n 'elm-watch'
tmux send-keys -t $S:$I "cd client/" C-m
tmux send-keys -t $S:$I "while (true ) ; do elm make gamecontrol.elm snl.elm --output snl.js ; inotifywait *elm ; done" C-m

inc
tmux new-window -t $S:$I -n 'emacs'
tmux send-keys -t $S:$I "emacs -nw ." C-m

inc
tmux new-window -t $S:$I -n 'psql'
tmux send-keys -t $S:$I "psql snl" C-m

tmux attach-ses -t $S
