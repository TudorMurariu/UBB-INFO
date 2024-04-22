import zipfile


def crack_password(keylist, archive):
    idx = 0

    with open(keylist, 'rb') as file:
        for line in file:
            for word in line.split():
                try:
                    idx += 1
                    archive.extractall(pwd=word)
                    return True, word.decode()
                except:
                    continue
    return False, None



if __name__ == '__main__':
    password_list = "wordlist_extended.txt"
    zip_file = "subiecte8iunie.zip"
    archived_file = zipfile.ZipFile(zip_file)

    cnt = len(list(open(password_list, "rb")))
    print("PASSWORDS IN FILE |", cnt)
    print("             FILE |", zip_file)

    state, key = crack_password(password_list, archived_file)
    if state:
        print("\033[96m          SUCCESS\033[0m |", key)
    else:
        print("\033[91m[!] PASSWORD NOT FOUND\n \033[0m")