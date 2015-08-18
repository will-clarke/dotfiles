#!/usr/bin/python
import re, subprocess, os

FOLDER_MAP = {
    "drafts":  "[Gmail]/Drafts",
    "sent":    "[Gmail]/Sent Mail",
    "flagged": "[Gmail]/Starred",
    "trash":   "[Gmail]/Trash",
    "archive": "[Gmail]/All Mail"
}

INVERSE_FOLDER_MAP = {v:k for k,v in FOLDER_MAP.items()}

INCLUDED_FOLDERS = ["INBOX"] + FOLDER_MAP.values()

def local_folder_to_gmail_folder(folder):
    return FOLDER_MAP.get(folder, folder)

def gmail_folder_to_local_folder(folder):
    return INVERSE_FOLDER_MAP.get(folder, folder)

def should_include_folder(folder):
    return folder in INCLUDED_FOLDERS

def get_secret_vars(name):
    return os.environ[name.upper()]

# remotepasseval = get_secret_vars('wmmclarke_gmail_app')

def get_keychain_pass(account=None, server=None):
    params = {
        'security': '/usr/bin/security',
        'command': 'find-internet-password',
        'account': account,
        'server': server,
        'keychain': '/Users/`whoami`/Library/Keychains/login.keychain',
    }
    command = "sudo -u `whoami` %(security)s -v %(command)s -g -a %(account)s -s %(server)s %(keychain)s" % params
    output = subprocess.check_output(command, shell=True, stderr=subprocess.STDOUT)
    outtext = [l for l in output.splitlines()
               if l.startswith('password: ')][0]
    return re.match(r'password: "(.*)"', outtext).group(1)


def get_password(name):
    try:
        output = subprocess.check_output(["pass", name])
        return output.split("\n")[0]
    except subprocess.CalledProcessError:
        return None
