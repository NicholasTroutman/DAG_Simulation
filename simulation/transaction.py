from collections import defaultdict

class Transaction:
    def __init__(self, _arrival_time, _counter, _numAgents): #initialize with numAgents for seen variable
            self.arrival_time = _arrival_time
            self.id = _counter
            self.agent = None
            self.seen=[""]*_numAgents#list where index=agent and value=time seen, for latency statistics

            #For tip selection and calculating confirmation_confidence
            self.cum_weight = 1
            self.cum_weight_multiple_agents  = defaultdict(lambda: 1)
            self.exit_probability = 0
            self.exit_probability_multiple_agents  = defaultdict(lambda: 0)
            self.confirmation_confidence = 0
            self.confirmation_confidence_multiple_agents = defaultdict(lambda: 0)

            #For performance statistics
            self.weight_update_time = 0
            self.tip_selection_time = 0

    def __str__(self):
        return str(self.id)

    def __repr__(self):
        return str(self.id)

    def set_weight_update_time(self, weight_update_time):
        self.weight_update_time = weight_update_time

    def set_tip_selection_time(self, tip_selection_time):
        self.tip_selection_time = tip_selection_time


    # def __init__(self, _arrival_time, _counter):
        # self.arrival_time = _arrival_time
        # self.id = _counter
        # self.agent = None
        # self.seen=[] #list where index=agent and value=time seen, for latency statistics

        # #For tip selection and calculating confirmation_confidence
        # self.cum_weight = 1
        # self.cum_weight_multiple_agents  = defaultdict(lambda: 1)
        # self.exit_probability = 0
        # self.exit_probability_multiple_agents  = defaultdict(lambda: 0)
        # self.confirmation_confidence = 0
        # self.confirmation_confidence_multiple_agents = defaultdict(lambda: 0)

        # #For performance statistics
        # self.weight_update_time = 0
        # self.tip_selection_time = 0