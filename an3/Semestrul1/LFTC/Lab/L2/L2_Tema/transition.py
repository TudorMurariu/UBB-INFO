class Transition:
    def __init__(self, source_state, destination_state, value):
        self.value = value
        self.source_state = source_state
        self.destination_state = destination_state

    def get_value(self):
        return self.value

    def get_source_state(self):
        return self.source_state

    def get_destination_state(self):
        return self.destination_state
