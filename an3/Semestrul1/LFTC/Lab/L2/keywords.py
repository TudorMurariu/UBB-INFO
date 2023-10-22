## clean csv
# lines = None
# with open('keywords.csv', 'r', encoding='utf-8') as f:
#     lines = f.readlines()

# with open('keywords.csv', 'w', encoding='utf-8') as f:
#     for line in lines:
#         line = line.replace(' ', '')
#         f.write(line)

def getKeywords(filename: str):
    keywords = {}
    with open(filename, 'r', encoding='utf-8') as f:
        lines = f.readlines()
        for line in lines:
            line = line.replace('\n', '')
            key, val = line.split(':')
            keywords[key] = val
    return keywords