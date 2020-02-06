import json

with open('entries.json') as f:
    data = json.load(f)

allnames = {}
for version, categories in data.items():
    names = {}
    for category, entries in categories.items():
        for entry in entries:
            paren = entry.find(' (')
            if paren != -1:
                trimmed = entry[:paren]
            else:
                trimmed = entry
            if not(trimmed in names):
                names[trimmed] = []
            names[trimmed].append((entry, category))
    allnames[version] = names

multinames = {version: {k: v for k, v in names.items() if len(v)>1}
              for version, names in allnames.items()}

with open('multientries.json', 'w') as f:
    json.dump(multinames, f, indent=4)
