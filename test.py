
coronavirus_letters = set('coronavirus')
for letter in coronavirus_letters:
    print("coronavirus contains", letter)

medications = ['remdesivir', 'hydroxychloroquine', 'kaletra', 'favipiravir']
diseases = ['coronavirus', 'hepatitis', 'malaria', 'influenza']

for med in medications:
    for disease in diseases:
        print("Can", med, "treat", disease, "?")

for letter in coronavirus_letters:
    for med in medications:
        if letter in med:
            print(letter, "is in coronavirus and also in", med)
