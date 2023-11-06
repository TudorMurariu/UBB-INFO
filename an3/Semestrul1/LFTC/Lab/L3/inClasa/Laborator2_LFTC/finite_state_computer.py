class FiniteStateMachine:
    def __init__(self, alphabet, states, initial_state, transitions, final_states):
        self.alphabet = alphabet
        self.states = states
        self.initial_state = initial_state
        self.transitions = transitions
        self.final_states = final_states

    def get_alphabet(self):
        return self.alphabet

    def get_transitions(self):
        return self.transitions

    def get_final_states(self):
        return self.final_states

    def get_states(self):
        return self.states

    def check_sequence(self, sequence):
        prefix = ""
        current_state = self.initial_state

        while sequence:
            found = False
            for transition in self.transitions:
                if (
                    transition.get_source_state() == current_state
                    and transition.get_value() == sequence[: len(transition.get_value())]
                ):
                    prefix += transition.get_value()
                    sequence = sequence[len(transition.get_value()) :]
                    current_state = transition.get_destination_state()
                    found = True
                    break
            if not found:
                return False

        if current_state in self.final_states:
            return True

        return False

    def get_longest_prefix(self, sequence):
        prefix = ""
        current_state = self.initial_state

        while sequence:
            found = False
            for transition in self.transitions:
                if (
                    transition.get_source_state() == current_state
                    and transition.get_value() == sequence[: len(transition.get_value())]
                ):
                    prefix += transition.get_value()
                    sequence = sequence[len(transition.get_value()) :]
                    current_state = transition.get_destination_state()
                    found = True
                    break
            if not found:
                return prefix

        return prefix