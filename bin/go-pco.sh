#!/bin/bash

echo 'Starting pco-box...'
box start

echo 'Starting eslint_d'
eslint_d start

cd ~/Code/people
tmux new-session -s pco -d
tmux split-window -h -t 1
tmux new-window -t pco -n logs
tmux new-window -t pco -n honcho
tmux send-keys -t pco:2 'tail -f log/development.log' C-m
tmux send-keys -t pco:3 'cd ../all-the-procfiles && froman -c honcho.yml' C-m
tmux select-window -t pco:1
tmux attach -t pco
