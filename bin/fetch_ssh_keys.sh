#!/bin/bash
# Fetches my ssh keys using gdrive cli

files=$(drive list)
brh_key_id=$(echo "$files" | grep "brh-key " | awk '{print $1}')
id_rsa_id=$(echo "$files" | grep "id_rsa " | awk '{print $1}')

cd ~/.ssh
drive download --id "$brh_key_id" --force
drive download --id "$id_rsa_id" --force

chmod 0600 ~/.ssh/brh-key ~/.ssh/id_rsa
