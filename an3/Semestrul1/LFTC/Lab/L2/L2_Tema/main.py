from finite_state_computer import FiniteStateMachine
from transition import Transition


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


def read_from_cmd():
    alphabet = input("Alfabetul: ").split(",")
    states = input("Starile: ").split(",")
    initial_state = input("Starea initiala: ")
    final_states = input("Starile finale: ").split(",")

    transitions = []
    n = int(input("Numarul de tranzitii: "))

    for _ in range(n):
        line = input("Sursa, destinatia si valoarea tranzitiei: ")
        source_state, line = line.split(",", 1)
        destination_state, value = line.split(",", 1)
        transitions.append(Transition(source_state, destination_state, value.strip()))

    return alphabet, states, initial_state, final_states, transitions


def print_states(finite_state_machine):
    print("Starile:", " ".join(finite_state_machine.get_states()))


def print_alphabet(finite_state_machine):
    print("Alfabetul:", " ".join(finite_state_machine.get_alphabet()))


def print_transitions(finite_state_machine):
    print("Tranzitile:")
    for transition in finite_state_machine.get_transitions():
        print("   ", transition.get_source_state(), transition.get_destination_state(), transition.get_value())


def print_final_states(finite_state_machine):
    print("Starile finale:", " ".join(finite_state_machine.get_final_states()))


def check_sequence(finite_state_machine):
    sequence = input("Secventa de verificat: ")
    if finite_state_machine.check_sequence(sequence):
        print("Secventa valida")
    else:
        print("Secventa invalida")


def print_longest_prefix(finite_state_machine):
    sequence = input("Secventa: ")
    prefix = finite_state_machine.get_longest_prefix(sequence)
    if not prefix:
        print("Nu e o secventa valida")
    else:
        print(prefix)


def print_read_commands():
    print("   0 - Iesire")
    print("   1 - Citire din fisier")
    print("   2 - Citire din cmd\n")


def print_commands():
    print("Options:")
    print("   0 - Iesire")
    print("   1 - Starile")
    print("   2 - Alfabetul")
    print("   3 - Tranzitile")
    print("   4 - Starea finala")
    print("   5 - Verifica validitatea secventei")
    print("   6 - Cel mai lung prefix")


if __name__ == "__main__":
    file_name = "constants.txt"
    command = int(input("Alege:\n1 - Citeste din fisier\n2 - Citeste din cmd\n0 - Iesire\n> "))
    if command == 0:
        exit(0)
    elif command == 1:
        alphabet, states, initial_state, final_states, transitions = read_from_file(file_name)
    elif command == 2:
        alphabet, states, initial_state, final_states, transitions = read_from_cmd()
    else:
        print("Comanda invalida!")
        exit(0)

    finite_state_machine = FiniteStateMachine(alphabet, states, initial_state, transitions, final_states)
    while True:
        print_commands()
        command = int(input("> "))
        if command == 0:
            exit(0)
        elif command == 1:
            print_states(finite_state_machine)
        elif command == 2:
            print_alphabet(finite_state_machine)
        elif command == 3:
            print_transitions(finite_state_machine)
        elif command == 4:
            print_final_states(finite_state_machine)
        elif command == 5:
            check_sequence(finite_state_machine)
        elif command == 6:
            print_longest_prefix(finite_state_machine)
        else:
            print("Comanda invalida!")
            exit(0)
        print("\n")

