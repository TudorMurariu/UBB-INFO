reserved_words = ['❓', 'int', 'float', 'string', '🔁', 'citire', 'altfel', 'main', 'print', '🏁']

operators = ['+', '-', '*', '%', '\\', '==', '>', '<', '>=', '<=', '!=', '⬅️']

separators = ['[', ']', '{', '}', '(', ')', ';', ' ', ',', '{', '}']

everything = separators + operators + reserved_words

codification = dict([(everything[i], i + 3) for i in range(len(everything))])

codification['identifier'] = 0
codification['integer_constant'] = 1
codification['real_constant'] = 2
codification['constant'] = 3