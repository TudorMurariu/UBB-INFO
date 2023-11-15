import re

from model.specs import *
from af.finite_state_computer import FiniteStateMachine
from af.transition import Transition

def split_line(words, line):
    while line:
        word, _, line = line.partition(',')
        words.append(word)

def read_from_file(file_name):
    alphabet = []
    states = []
    initial_state = ""
    final_states = []
    transitions = []

    with open(file_name, "r") as fin:
        split_line(alphabet, fin.readline().strip())
        split_line(states, fin.readline().strip())
        initial_state = fin.readline().strip()
        split_line(final_states, fin.readline().strip())

        for line in fin:
            source_state, line = line.split(",", 1)
            destination_state, value = line.split(",", 1)
            transitions.append(Transition(source_state, destination_state, value.strip()))

    return alphabet, states, initial_state, final_states, transitions

def isPartOfOperator(line, index):
    if index + 1 < len(line):
        if line[index] == '-' and line[index+1] >= '0' and line[index+1] <= '9':
            if index - 1 >= 0:
                if(line[index-1] in separators + operators):
                    return False
                else:
                    return True
    for op in operators:
        if line[index] in op:
            return True
    return False


def isEscapedQuote(line, index):
    return False if index == 0 else line[index - 1] == '\\'


def getStringToken(line, index):
    token = ''
    quoteCount = 0

    while index < len(line) and quoteCount < 2:
        if line[index] == '"' and not isEscapedQuote(line, index):
            quoteCount += 1
        token += line[index]
        index += 1

    return token, index

def getNegativeToken(line,index):
    token = '-'
    index += 1

def getOperatorToken(line, index):
    token = ''

    while index < len(line) and isPartOfOperator(line, index):
        token += line[index]
        index += 1

    return token, index


def tokenGenerator(line, separators):
    token = ''
    index = 0

    while index < len(line):
        if line[index] == '"':
            if token:
                yield token
            token, index = getStringToken(line, index)
            yield token
            token = ''
        elif isPartOfOperator(line, index):
            if token:
                yield token
            token, index = getOperatorToken(line, index)
            yield token
            token = ''

        elif line[index] in separators:
            if token:
                yield token
            token, index = line[index], index + 1
            yield token
            token = ''

        else:
            token += line[index]
            index += 1
    if token:
        yield token


def isIdentifier(token):
    return re.match(r'^[a-zA-Z]([a-zA-Z]|[0-9]|_){,8}$', token) is not None


def isConstant(token):
    return re.match('^(0|[\+\-]?[1-9][0-9]*)$|^\'.\'$|^\".*\"$', token) is not None

def isIntegerAF(token):
    alphabet, states, initial_state, final_states, transitions = read_from_file("af/integer.txt")
    finite_state_machine = FiniteStateMachine(alphabet, states, initial_state, transitions, final_states)
    return finite_state_machine.check_sequence(token)


def isRealAF(token):
    alphabet, states, initial_state, final_states, transitions = read_from_file("af/real.txt")
    finite_state_machine = FiniteStateMachine(alphabet, states, initial_state, transitions, final_states)
    return finite_state_machine.check_sequence(token)

def isIdentifierAF(token):
    if len(token) > 8:
        raise Exception("More than 8 characters")
        # return False
    alphabet, states, initial_state, final_states, transitions = read_from_file("af/identifier.txt")
    finite_state_machine = FiniteStateMachine(alphabet, states, initial_state, transitions, final_states)
    return finite_state_machine.check_sequence(token)
