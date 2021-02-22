import sqlite3
import sys
import os

def prettify_rows(rows):
    res = ""
    i = 0
    for row in rows:
        i += 1
        res += "{}) {}\n {}\n\n".format(i, row[0], row[1])
    return res

def search_en_word(conn, word):
    cur = conn.cursor()
    sql = """SELECT en_sentence, ru_sentence, INSTR(en_sentence, ?) hits FROM translations WHERE hits > 0 limit 10"""; # sql injection! nice :)
    s = (word,)
    cur.execute(sql, s)
    rows = cur.fetchall()
    return prettify_rows(rows)

def search_ru_word(conn, word):
    cur = conn.cursor()
    sql = """SELECT ru_sentence, en_sentence, INSTR(ru_sentence, ?) hits FROM translations WHERE hits > 0 limit 10"""; # sql injection! nice :)
    s = (word,)
    cur.execute(sql, s)
    rows = cur.fetchall()
    processed = []
    return prettify_rows(rows)

def main():
    if (len(sys.argv) > 2):
        lang = sys.argv[1]
        query = ""
        for i in range(2, len(sys.argv)):
            query += sys.argv[i] + " "

        print(query)
        path = os.path.dirname(os.path.abspath(__file__))
        db = os.path.join(path, 'open_subs_en_ru.db')

        if (lang == 'en'):
            conn = sqlite3.connect(db)
            res = search_en_word(conn, query)
            print(res)
        elif (lang == 'ru'):
            conn = sqlite3.connect(db)
            res = search_ru_word(conn, query)
            print(res)

main()
