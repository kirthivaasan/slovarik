import sqlite3
from sqlite3 import Error


def create_table(conn, create_table_sql):
    """ create a table from the create_table_sql statement
    :param conn: Connection object
    :param create_table_sql: a CREATE TABLE statement
    :return:
    """
    try:
        c = conn.cursor()
        c.execute(create_table_sql)
    except Error as e:
        print(e)

def create_translation(conn, translation):
    sql = ''' INSERT INTO translations(en_sentence,ru_sentence)
              VALUES(?,?) '''
    cur = conn.cursor()
    cur.execute(sql, translation)
    conn.commit()
    return cur.lastrowid

def create_batch_translation(conn, translations):
    sql = ''' INSERT INTO translations(en_sentence,ru_sentence)
              VALUES(?,?)'''
    conn.executemany(sql, translations)
    conn.commit()

def insert_data(conn):
    root_dir = 'en-ru.txt'
    en_file = open("{}/OpenSubtitles.en-ru.en".format(root_dir), 'r')
    ru_file = open("{}/OpenSubtitles.en-ru.ru".format(root_dir), 'r')

    NUM_LINES = 25910105
    BUF_SIZE = 5000000
    buf = []

    # translation_id = create_translation(conn, translation)
    # print(en_line)
    # print(ru_line)
    count = 0
    for i in range(NUM_LINES - (NUM_LINES%BUF_SIZE)):
        en_line = en_file.readline().rstrip()
        ru_line = ru_file.readline().rstrip()
        translation = (en_line, ru_line)
        buf.append(translation)

        if i%BUF_SIZE == 0:
            print('Inserted {} sentences.'.format(i))
            create_batch_translation(conn, buf)
            buf = []
        count += 1

    buf = []
    for i in range(NUM_LINES%BUF_SIZE):
        en_line = en_file.readline().rstrip()
        ru_line = ru_file.readline().rstrip()
        translation = (en_line, ru_line)
        buf.append(translation)
        count += 1
    create_batch_translation(conn, buf)
    print("Count {}".format(count))


def create_connection(db_file):
    """ create a database connection to a SQLite database """
    conn = None
    try:
        conn = sqlite3.connect(db_file)
        return conn
    except Error as e:
        print(e)
    return conn


if __name__ == '__main__':
    ROOT = "."
    OPUS_DB_NAME = "open_subs_en_ru.db"
    db_path = "{}/{}".format(ROOT, OPUS_DB_NAME)
    create_connection(db_path)

    sql_create_table = """CREATE TABLE IF NOT EXISTS translations (
                            id integer PRIMARY KEY,
                            en_sentence text NOT NULL,
                            ru_sentence text NOT NULL
                          );"""

    conn = create_connection(db_path)

    if conn is not None:
        create_table(conn, sql_create_table)
        insert_data(conn)
    else:
        print("Error! cannot create the database connection.")
