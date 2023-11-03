#Makes Map from grayscale image with red roads and blue intersections
import sys
import timeit
import random
import time
import math
from collections import Counter
import numpy as np
import networkx as nx
import matplotlib.pyplot as plt
from numpy.random import rand
from operator import add
import csv #write/read from csv
import os

## Functions to create graph of greyscale traffic image
import numpy as np
from matplotlib import pyplot as plt
import networkx as nx
from PIL import Image
import math

#returns Euclidean distance between two 2d poins
def Distance(n1,n2):
    return math.hypot(n1[0]-n2[0], n1[1]-n2[1])


#distance of c to a and b line
def DistanceToVector(a,b,c):
    a=np.array(a)
    b=np.array(b)
    c=np.array(c)
    return np.abs(np.cross(b-a, c-a) / np.linalg.norm(b-a))


#determines if pixel yx is perimeter of red circle (or close blue)
def isPerimeter(image, y, x):

    #print("isPerimeter:\t", y,", ",x)

    x2=x-2
    y2=y-2

    boxRange=5 #box to make


    red=False
    grey=False
    blue = False

    #quit()
    for i in range(0,boxRange): #x axis
        for j in range(0,boxRange): #y axis
            #print("\ny,x: ",y2+j,", ", x2+i,":\t",image[y2+j][x2+i])

            #print(image)

            #print(image[y2+j][x2+i][0])
            #print(image[y2+j][x2+i][1])
            #print(image[y2+j][x2+i][2])

            try: #may go out of bounds
                if image[y2+j][x2+i][0]>100+image[y2+j][x2+i][1]: #Red number found
                    #print("RED FOUND")
                    red=True
                    if red == True and grey == True: #if red and blue found, then this is perimeter
                        #print("ITS A PERIMTER")
                        #quit()
                        return True #perimeter

                elif image[y2+j][x2+i][2]>100+image[y2+j][x2+i][1]: #blue number found
                    #print("Blue Found")
                    blue = True #not needed
                else:
                    #print("Grey Found")
                    grey = True #not red or blue


            except IndexError:
                print("OOB")
                quit()
                #pass

    if red == True and grey == True: #if red and blue found, then this is perimeter
        #print("ITS A PERIMTER")
        #quit()
        return True #perimeter
    #quit()
    return False #not perimeter



#Recursive Function searching for perimeters then find_center
def findPerimeter(image, y, x, perimeterList):

    #print("\n\nfindPerimeter START:\t",y,", ",x, "\t\t",perimeterList   )
    ##find box around
    boxRange=21 #box to make
    y2=y-10
    x2=x-10

    #erimeterList = []
    tempPerimeterList = []

    ##check start
    #if (isPerimeter(image,y,x)):
    #    perimeterList.append([y,x]) #starting place


    #X max
    for i in [0,boxRange]: #x axis
        for j in range(0,boxRange): #y axis
            try:
                if isPerimeter(image,y2+j, x2+i):
                    #print("findPerimeter:\tfound perimeter")
                    tempPerimeterList.append([y2+j,x2+i])
            except IndexError:
                print("INDEX ERROR")
                quit()
                pass

    #Y max
    for i in range(0,boxRange): #x axis
        for j in [0, boxRange]: #y axis
            try:
                if isPerimeter(image,y2+j, x2+i):
                    tempPerimeterList.append([y2+j,x2+i])
            except IndexError:
                print("INDEX ERROR")
                quit()
                pass

    #print("\n\nfindPerimeter:\ttempPerimeterList:\t",tempPerimeterList)



    #print("\nfindPerimeter:\tBegin Distance Calcs:\t",perimeterList )
    tooClose = False;
    for t in tempPerimeterList: #for every new perimeter Pixel
        tooClose = False;
        for p in perimeterList: #for every validated perimeter pixel

            #print([t[0], t[1]]," - ",[p[0], p[1] ]," =\t", Distance( [t[0], t[1]], [p[0], p[1] ])) #distance between two pixels
            if Distance( [t[0], t[1]], [p[0], p[1] ]) <10: #if too close,
                tooClose = True #set flag

        if tooClose == False:
            #print("Not too Close")
            perimeterList.append([t[0],t[1]]) #add to list
            #todo=input()
            perimeterList = findPerimeter(image, t[0], t[1], perimeterList) #RECURSION

    return perimeterList #


##defnct find center operations
#def FindCenter(image, y, x):
#    x2=x-20
#    boxRange = 70 #was 40 for DCRedBlue
#    reds=[]

    #find center of red dot
#    for i in range(0,boxRange):
#        for j in range(0,boxRange):
#            try:
#                if image[y+j][x2+i][0]>100+image[y+j][x2+i][1]: #Red number found
#                    reds.append([y+j,x2+i])
#            except IndexError:
                #print("OOB")
#                pass


#    result=np.mean(reds, axis=0)
    #print(result)

#    return [int(result[0]), int(result[1])]z



#find blue lines around an intersection
def FindEdges(image, intersections):
    print("\n\nFindEdges FUNC START\n")
    #start at intersection
    allFinal=[] #[[index,[blues]]]
    print("\nNUMBER OF INTERSECTIONS:\t",intersections)
    for index, yx in intersections:
        #print(index)
        #print(yx)

        #trace rectangle around old, 100x100 box
        #y2=yx[0]-50
        #x2=yx[1]-50
        y2=yx[0]-25
        x2=yx[1]-25
        blues=[]

        #for i in [0,100]: #old, 100x100 box
        #    for j in range(0,100): #old, 100x100 box
        for i in [0,50]: #old, 100x100 box
            for j in range(0,50): #old, 100x100 box
                try:
                    #print("y: ",y2+j," x: ",x2+i)
                    if image[y2+j][x2+i][2]>100+image[y2+j][x2+i][1]: #blue number found
                        blues.append([y2+j,x2+i])
                except IndexError:
                    #print("OOB")
                    pass

        #for i in range(0,100):
        #    for j in [0,100]:
        for i in range(0,20):
            for j in [0,50]:
                try:
                    if image[y2+j][x2+i][2]>100+image[y2+j][x2+i][1]: #Blue number found)
                        blues.append([y2+j,x2+i])
                except IndexError:
                    #print("OOB")
                    pass

        #get rid of similar blues
        #print(blues)
        final=[]
        while len(blues)>0: #O(n**2)
            same=[]
            same.append(blues[0])
            for b in blues:
                if Distance(blues[0],b)<5: #too similar
                    same.append(b)

            final.append(np.mean(same, axis=0))
            blues=[b for b in blues if b not in same]
            #print(blues)

        #print(final)

        finalPretty= [[int(x),int(y)] for x,y in final]
        allFinal.append([index,finalPretty])

        #for i in allFinal:
            #print("Intersection: ",i,"\n")

        #print("\n\n")

        #pass all blues to IdentifyBlueEdgeIntersections
        neighbors=[] #[[index, [neighbpors]]]
        for index, blues in allFinal:
            n=IdentfiyBlueEdgeIntersection(intersections, intersections[index],blues)
            neighbors.append([index,n])

    #print("FINDEDGES FUNC END\n")
    return neighbors #integer response



#find which intersection the edge points to given intesection, bluepoint, and other intersections
def IdentfiyBlueEdgeIntersection(intersections, intersection, bluepoint ):
    #
    neighbors=[] #[index, distance]


    for bp in bluepoint: #for each blue edge
        #print("\nblueedge: ",bp)

        closestIntersection=[-1, 9999999999999999]

        for index, yx  in intersections: #for each other intersections

            if index!=intersection[0]: #not same as origin
                if DistanceToVector(intersection[1], bp, yx) < 30: #close enough to match ##USED TO BE 50
                    distance=Distance(yx,intersection[1]) #measure distance
                    #print("\n")
                    #print(index)
                    #print("Distance: ",distance)
                    #print("D to V: ",DistanceToVector(intersection[1], bp, yx))
                    #print("CI: ",closestIntersection[1])


                    if Distance(yx, bp) < Distance(yx,intersection[1]):#correct direction
                        #print("Correct Direction")

                        if distance<closestIntersection[1]: #closest to originating intersection
                            closestIntersection=[index, distance]

        neighbors.append(closestIntersection)
        #print("\nNeighbors: ",neighbors)
    return neighbors




#load from ./maps/image.csv
def loadGraph(np_frame, file_name):

    edge=0

    print("loading: ",file_name)
    G = nx.Graph()

    with open(file_name, mode='r') as file:
        csvFile = csv.reader(file)
        next(csvFile)
        for lines in csvFile:
            if "Node1ID" in lines:
                #next(csvFile) #skip second header
                #print("EDGE HEADER FOUND")
                edge=1

            elif edge==0:
                #print("edge==0")
                #print(lines)
                #print("[1]: ",lines[1])
                coord = lines[1].replace(",", "").replace("(","").replace(')','') #remove non-numeric digits "###, ##"
                coordX, coordY = [int(i) for i in coord.split() if i.isdigit()] #split into 2 vars
                #coord = ( int(lines[1][0]), int(lines[1][1]) )


                G.add_node(int(lines[0]), pos=(coordX, coordY))


            elif edge==1:
                #print("edge==1")
                #print(lines)
                G.add_edge(int(lines[0]), int(lines[1]), weight=int(lines[2]))


    #print("LOADED GRAPH RESULTS\n\n")
    ##for i in G.nodes.data():
     #   print(i)

    #for i in G.edges.data():
     #   print(i)
    #print(G.nodes.data())
    #print(G.edges.data())
    return G #return networkx Graph




#save to ./maps/image.csv
def saveGraph(G, file_name):
    #check if file already exists
    if os.path.isfile(file_name): #
        #print("CSV IS HERE\n\n\n\n")
        print(file_name, " Already exists, (saveGraph) ERORR!!!\n\n")
        pass
    else: #doesn't exist leave alone
       # print("CSV IS NOT HERE\n\n\n\n")
        with open(file_name, 'w', newline='') as file:
            print("WRITING GRAPH CSV")
            writer=csv.writer(file, dialect='excel')


            header=["NodeID", "Coordinates"]
            writer.writerow(header) #werite header

            #write nodeId, coord
            for nodeID, coord in G.nodes.data():
                line=[]
                line.append(nodeID)
                line.append(coord['pos'])
                writer.writerow(line)

            header2=["Node1ID","Node2ID","Weight"]
            writer.writerow(header2)
            for n1, n2, weight in G.edges(data=True):
                line=[]
                line.append(n1)
                line.append(n2)
                line.append(weight['weight'])
                writer.writerow(line)


    #print("Writing GRAPH RESULTS\n\n")
    #for i in G.nodes.data():
    #    print(i)

   # for i in G.edges.data():
    #    print(i)




##take image, return full graph with nodes + edges
def LoadImageIntoGraph(image):
    print("LOAD IMAGE INTO GRAPH\n\n")
    im_frame = Image.open(image)
    np_frame = np.array(im_frame.getdata())
    np_frame.shape=(int(np_frame.shape[0]/im_frame.width), im_frame.width,  3) #x,y of the image

    #check for graph.csv existence
    dir_name='./maps/'



    csvFile=image[:-4]+"Graph.csv" #DCRedBlue.csv
    #print("Saving to file: ",csvFile)

    file_name = os.path.join(dir_name, csvFile) #root/maps/
    #print(file_name)
    #print("Saving to EXACT FILE: ",file_name)
    #todo check that the file exists and don't do this if it already exists



    #check if file already exists
    if os.path.isfile(file_name): #
        #print("CSV IS HERE\n\n\n\n")
        print(file_name, " Already exists, Reading (LoadImageIntoGraph)\n\n")

        #G = nx.Graph() #networkx graph

        G = loadGraph(np_frame, file_name)

        return G, np_frame #return G, np_frame

    else: #doesn't exist leave alone
        print(file_name," Doesn't Exist, Creating ")

    #plt.imshow(np_frame)
    #plt.show()


        intersections=[] #nodes
        for y in range(10,np_frame.shape[0], 2): #skip every other line
            for x in range(10,np_frame.shape[1], 2): #skip every other line
                if np_frame[y][x][0]>np_frame[y][x][1]+100: #Is it Red?
                    isolated=True
                  #
                  #check if new point is too close existing intersections
                    for index, yx in intersections:
                        if Distance(yx,[y,x]) <50:
                            isolated=False

                    if isolated == True:
                        #print("RED:\t", y,", ",x) #
                        #quit()
                        ##find center
                        if isPerimeter(np_frame,y,x): #is this red pixel a perimeter?
                            #print("LoadImageIntoGraph: This is red, and a perimeter\n")
                            perimeterList=[[y,x]] #add to pList
                            #print("LoadImageIntoGraph: perimeterList:\t",perimeterList)
                            perimeterList = findPerimeter(np_frame,y,x,perimeterList) #find all perimeters of circle
                            #print("LoadImageIntoGraph: AFTER FINDPERIMETER() (Should be Ring) -- perimeterList:\t",perimeterList)

                            #find mean of perimeter, result should be center of red circle
                            result=np.mean(perimeterList, axis=0)
                            tooClose=False
                            for id, yx in intersections:
                                if Distance([int(result[0]), int(result[1])], yx) < 50:
                                    #print("TOO CLOSE")
                                    tooClose = True #too close can't add

                            if tooClose == False: #isolated enough
                                intersections.append([len(intersections),[int(result[0]), int(result[1])]]) #maybe backwards


        #print("LoadImageIntoGraph: result\t", intersections)
        for id, yx in intersections:
            print(id,":\t",yx)


        #for count, i in enumerate(intersections):
            #print("Intersection [",count,"]: ",i)
        #for i in range(0,len(intersections)):
        #    print(i,": ",intersections[i][1])

        ##plot intersections
        #plt.imshow(np_frame)
        #for index, yx in intersections:
        #    plt.scatter(yx[1],yx[0],s=40,color="green")
        #    plt.annotate(index,[yx[1],yx[0]])
        #plt.show()

        #get edges
        #quit()
        edges=FindEdges(np_frame, intersections) #Edges= [[index, [neighbors]]] #neighbors=[index,distance]

        G = nx.Graph()

        #create nodes
        for index, yx in intersections:
            G.add_node(index,pos=(yx[1],yx[0]))

        #create edges
        for index, edge in edges:
            for index2, distance in edge:
                if index2>index: ##for only 1 edge between
                    G.add_edge(index,index2,weight=int(distance))
                    #print(index," <--> ", index2," ~ ",int(distance))


        saveGraph(G, file_name) #write to csv


        return G, np_frame #graph, image


##https://stackoverflow.com/questions/328107/how-can-you-determine-a-point-is-between-two-other-points-on-a-line-segment
def isBetween(a, b, c):
    #print("\nisBetween:")
    #print(a)
    #print(b)
    #print(c)

    if (abs(Distance(a,b)+Distance(b,c)-Distance(a,c))<1): #epsiolon=1
        #print("IS BETWEEN")
        return True
    #print("NOT BETWEEN")
    return False
