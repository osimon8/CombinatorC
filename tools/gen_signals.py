import urllib.request
import re
import json 
import sys

base_url = 'https://raw.githubusercontent.com/wube/factorio-data/master/base/prototypes/'

files = [('signal', '', 3), ('item', 'item-', 0), ('fluid', 'fluid-', 0)] # 3 to exclude wildcards, they are first

def delete_nested(s):
    acc = ""
    line = ""
    ctr = 0
    for line in s.split("\n"):
        m = re.match(r"\s*(\S*)\s*:\s*(.*)", line)
        decs = 0
        if m is None:
            for c in line:
                if c == '{':
                    ctr += 1 
                elif c == '}':
                    decs += 1
            if ctr == 1:
                acc += line + "\n"
            ctr -= decs
            continue

        value = m[2]
        if value == "":
            continue
        for c in value:
            if c == '{':
                ctr += 1 
            elif c == '}':
                decs += 1

        if ctr == 1:
            acc += line + "\n" 
        ctr -= decs
    return acc  

out_data = []

for f, prefix, start_index in files:
    url = base_url + f + '.lua'
    with urllib.request.urlopen(url) as data: 
        raw = "".join([l.decode('utf-8') for l in data.readlines()])
        def repl(m):
            return m[1] + "\"" + m[2] + "\": " + m[3] 
        raw = re.sub(r"\s(.*--.*)\n", lambda _ : "\n", raw) # delete comments 
        raw = re.sub(r".*\(\).*\n", lambda _ : "\n", raw) # delete lines with function calls
        raw = re.sub(r"([,\{]\s*)(\S*)\s*=([^,\}]*)", repl, raw)
        regex = re.match(r".*data:extend\(\s*\{(.*)\}\s*\).*", raw, re.DOTALL)
        try:
            data = regex[1]
        except: 
            print(f"Couldn't parse '%s'" % url)
            exit(1)
        data = delete_nested(data)
        data = re.sub(r",(\s*\})", lambda m : m[1], data)
        data = '[' + data + ']'
        js = json.loads(data)
        dump = []
        for entry in js:
            dump.append(prefix + entry['name'])
        data = dump[start_index:]
        data.sort() # puts lower priority virtual signals first, rest is irrelevant
        out_data.extend(data)

args = sys.argv
if len(args) > 1 and args[1] == '--ocaml':
    out = json.dumps(out_data).replace(",", ";\n")
    print(out)
else:    
    print("\n".join(out_data))

