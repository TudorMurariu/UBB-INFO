from FIP import getFIP
from TS import getTS
from keywords import getKeywords

keywords = getKeywords('keywords.csv')

cod = ""
with open('cod.cpp', 'r', encoding='utf-8') as f:
    cod = f.read()

ts, exception = getTS(cod, keywords)

# Print the TS table
print('TS')
print('--------------------')
ts.inorder_traversal_print()
print('--------------------')
print()


fip, exception2 = getFIP(cod, keywords, ts)

# print the FIP table
print('FIP')
print('--------------------')
for key in fip.keys():
    print(key + " : " + fip[key])
print('--------------------')

if exception != None:
    raise exception

if exception2 != None:
    raise exception2