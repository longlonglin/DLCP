# -*- coding:utf-8 -*-
import datetime
from collections import deque
class Graph:

    def __init__(self,path):
        self.path = path
        self.sadj_list,self.tadj_list,self.tmin,self.tmax=self.TemporalGraph()        
   
    def TemporalGraph(self): #在数据预处理的时候将graph处理成t=0开始这样减小t的存储空间
        tmin,tmax,sadj_list,tadj_list = 1000000000000,-1,{},{}
        print(self.path + " is loading...")
        with open(self.path, 'r') as file: 
            lines=file.readlines()
            for line in lines:
                line = line.split()
                from_id, to_id, time_id = int(line[0]), int(line[1]), int(line[2])
                if from_id == to_id:
                    continue
                if time_id > tmax:
                    tmax = time_id
                if time_id < tmin:
                    tmin = time_id
                for (f_id, t_id) in [(from_id, to_id),(to_id, from_id)]:#有向变无向.
                    if f_id in sadj_list:
                        if t_id not in sadj_list[f_id]:
                            sadj_list[f_id].append(t_id)
                    else:
                        sadj_list[f_id] = [t_id] #采用list较set内存消耗小 
                    if f_id in tadj_list:
                        if time_id in tadj_list[f_id]:
                            if t_id not in tadj_list[f_id][time_id]:
                                tadj_list[f_id][time_id].append(t_id)
                        else:
                            tadj_list[f_id][time_id] = [t_id]
                    else:
                        tadj_list[f_id] = {}
                        tadj_list[f_id][time_id] = [t_id]
        return (sadj_list,tadj_list, tmin,tmax)  
        
    def kcore(self,adj):
        q = []
        D = []
        deg = {}
        for node_id in adj:
            deg[node_id]=len(adj[node_id])
            if deg[node_id] < self.k:
                q.append(node_id)
        while (len(q) > 0):
            v = q.pop()
            D.append(v)
            for w in adj[v]:
                if deg[w] >= self.k:
                    deg[w] = deg[w] - 1
                    if deg[w] < self.k:
                        q.append(w)
        Vc = adj.keys()-set(D)
        newadj={}
        for v in Vc:
            newadj[v]=set(adj[v])&Vc
        return Vc,newadj

    def intersection_graph(self,tadj_list, interval,nodes):
        intersection_adj = {}
        interval_iterator = range(interval[0] + 1, interval[1] + 1)
        for node in nodes:
            if interval[0] in tadj_list[node]:
                intersection_adj[node] =set(tadj_list[node][interval[0]])&nodes
            else:
                continue
            for timestamp in interval_iterator:
                if timestamp in tadj_list[node]:
                    intersection_adj[node]&=set(tadj_list[node][timestamp])
                else:
                    del intersection_adj[node]
                    break
        Vc,newadj=self.kcore(intersection_adj)

        return [interval,Vc,newadj]

    def Enum(self,k,sigma):
        self.k, self.sigma = k, sigma
        V,newadj=self.kcore(self.sadj_list)
        LC,Q= [],deque()  
        for timestamp in range(self.tmin,self.tmax+1-self.sigma+1):
            temp=self.intersection_graph(self.tadj_list,(timestamp,timestamp+self.sigma-1),V)
            if len(temp[1])>0:
                Q.append(temp)
        while len(Q)>0:
            l=len(Q)
            if l==1:
                r=Q.popleft()
                LC.append(r)
                break
            R1=Q.popleft()
            LC.append(R1)
            for _ in range(l-1):              
                R2 = Q.popleft()
                LC.append(R2)
                if R1[0][1]>=R2[0][0]:
                    nodelist=R1[1]&R2[1]
                    temp=self.intersection_graph(self.tadj_list,(R1[0][0],R2[0][1]),nodelist)
                    if len(temp[1])>0:
                        Q.append(temp)
                R1=R2
        for res in LC[:]:
            if  40533 not in res[1]:
                LC.remove(res)
        for result in LC[:]: #maximal check
            for candi in LC[:]: 
                if candi!=result and set(result[1]).issubset(set(candi[1])) and candi[0][0]<=result[0][0] and candi[0][1]>=result[0][1]:
                    LC.remove(result)
                    break
        return LC
   
    def GreLC(self,k,sigma,r):
        self.k, self.sigma, self.r= k, sigma,r
        print(self.path+"grelc")
        LC=self.Enum(k,sigma)
        R=[]
        if len(LC)<=self.r:
            R=LC
        else:
            max=-1
            for result in LC:
                if (result[0][1]-result[0][0]+1)*len(result[1])>max:
                    temp=result
                    max=(result[0][1]-result[0][0]+1)*len(result[1])
            R.append(temp)
            LC.remove(temp)
            for _ in range(self.r-1):
                max=-1
                b=len(self.coverage_measure(R))
                for result in LC:
                    R.append([result[0],result[1]])
                    a=len(self.coverage_measure(R))
                    R.remove([result[0],result[1]])
                    size=a-b
                    if size>max:
                        temp=result
                        max=size
                R.append(temp)
                LC.remove(temp)
        for res in R:
            adj={}
            temp_set=set(res[2][40533])
            temp_set.add(40533)
            for v in temp_set:
                adj[v]=res[2][v]&temp_set                
            vc,new_adj=self.kcore(adj)   
            for v in new_adj:
                for w in new_adj[v]:
                    print(v,w,res[0])      
                    
    def coverage_measure(self,can_R): 
        coverage_set=set()
        for x in can_R:
            for t in range(x[0][0],x[0][1]+1): 
                for v in x[1]:
                    coverage_set.add((v,t))   
        return coverage_set

    def kcore_test(self,k): #包含查询顶点的连通kcore的顶点数
        self.k=k
        Vc,adj=self.kcore(self.sadj_list)
        q=[40533]
        seen={40533}
        while q:
            v=q.pop()
            for u in adj[v]:
                if u not in seen:
                    q.append(u)
                    seen.add(u)
        print("kcore_lenth"+str(len(seen)))
        
if __name__ == '__main__':
    k=3
    print("query_nodes is 40533")
    G=Graph("dblp_case")
    G.kcore_test(k)

    
        