#!/usr/bin/env python3

import re, subprocess, pathlib

def get_email_address(key):

    address_file = pathlib.Path.home() / ".email_addresses"
    email_addresses = address_file.read_text().splitlines()

    r = re.compile(key)

    for addr in email_addresses:
        if m := r.search(addr):
            return addr

    return None

def get_authinfo_password(login):
    res = subprocess.run("cat ~/.authinfo", shell=True, stdout=subprocess.PIPE)
    authinfo = res.stdout.decode().splitlines()

    r = re.compile(f"(?=.*login {login}.*).*password ([^ ]*).*")

    for row in authinfo:
        if m := r.search(row):
            return m.group(1)

    return None
