from simulation.node import Node
class Agent(Node):
    def __init__(self, counter):
        Node.__init__(self, counter)
        ##These variables are mobile agent specific
        #self.coordinates= [] #list of double x and y coordinates in double [x,y]
        self.past_coordinates=[]
        self.destination=[]
        self.prev_dest=[]
        self.vector=[]
        self.speed=15

        
        
        
##Don't know if these need to be specified
    #def __str__(self):
     #   return str(self.id)

    #def __repr__(self):
      #  return str(self.id)
