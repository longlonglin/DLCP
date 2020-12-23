# -*- coding:utf-8 -*-
import datetime
from collections import deque
class Graph:

	def __init__(self,path):
		self.path = path
		self.sadj_list,self.tadj_list,self.tmin,self.tmax=self.TemporalGraph()		
		print("number of nodes: " + str(len(self.sadj_list)))
		number1 = 0
		number2 = 0
		for u in self.sadj_list:
			number1 += len(self.sadj_list[u])
			for t in self.tadj_list[u]:
				number2 += len(self.tadj_list[u][t])
		print("number of edges: " + str(number1 / 2))
		print("number of temporal edges: " + str(number2 / 2))
		print("tmin: "+ str(self.tmin)+ "tmax: "+str(self.tmax)+"|T|: "+str(self.tmax-self.tmin+1))
		print("..........................................")		
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
				for (f_id, t_id) in [(from_id, to_id),(to_id, from_id)]: #有向变无向.
					if f_id in sadj_list:
						if t_id not in sadj_list[f_id]:
							sadj_list[f_id].append(t_id)
					else:
						sadj_list[f_id] = [t_id]  
					if f_id in tadj_list:
						if time_id in tadj_list[f_id]:
							if t_id not in tadj_list[f_id][time_id]:
								tadj_list[f_id][time_id].append(t_id)
						else:
							tadj_list[f_id][time_id] = [t_id] #采用list较set内存消耗小 
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
		return Vc

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
		Vc=self.kcore(intersection_adj)
		return [interval,Vc]

	def Naive(self,k,sigma):
		self.k, self.sigma = k, sigma
		print(self.path+"naive")
		starttime = datetime.datetime.now()
		LC=[]
		for time_gap in range(self.sigma,self.tmax-self.tmin+2):
			for timestamp in range(self.tmin,self.tmax+1-time_gap+1):
				temp= self.intersection_graph(self.tadj_list, (timestamp,timestamp+time_gap-1), set(self.sadj_list.keys()))
				if len(temp[1]) > 0:
					LC.append(temp)
		for result in LC[:]: #maximal check
			for candi in LC[:]: 
				if candi!=result and set(result[1]).issubset(set(candi[1])) and candi[0][0]<=result[0][0] and candi[0][1]>=result[0][1]:
					LC.remove(result)
					break
		endtime = datetime.datetime.now()
		print('time (s): ' + str(endtime - starttime))
		print(len(LC))

	def Enum(self,k,sigma):
		self.k, self.sigma = k, sigma
		Vc=self.kcore(self.sadj_list)
		intersection_graph_count=[0]
		LC,Q= [],deque()  
		for timestamp in range(self.tmin,self.tmax+1-self.sigma+1):
			temp=self.intersection_graph(self.tadj_list,(timestamp,timestamp+self.sigma-1),Vc)
			intersection_graph_count[0]=intersection_graph_count[0]+1
			if len(temp[1])>0:
				Q.append(temp)
		while Q:
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
					intersection_graph_count[0]=intersection_graph_count[0]+1
					if len(temp[1])>0:
						Q.append(temp)
				R1=R2
		candi_R=LC[:]
		for result in LC[:]: #maximal check
			for candi in LC: 
				if candi!=result and set(result[1]).issubset(set(candi[1])) and candi[0][0]<=result[0][0] and candi[0][1]>=result[0][1]:
					LC.remove(result)
					break
		print(len(LC))
		return LC, intersection_graph_count
	 
	def GreLC(self,k,sigma,r):
		self.k, self.sigma, self.r= k, sigma,r
		print(self.path+"grelc")
		print("efefffffffffffffffffffff")
		starttime = datetime.datetime.now()
		LC, intersection_graph_count=self.Enum(k,sigma)
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
		coverage=len(self.coverage_measure(R))
		endtime = datetime.datetime.now()
		print("R: "+str(len(R)))
		print(R)
		print("intersection_graph_count"+str(intersection_graph_count))
		print('time (s): ' + str(endtime - starttime)+"coverage"+str(coverage))
		
	def coverage_measure(self,can_R): 
		coverage_set=set()
		for [I,V] in can_R:
			for t in range(I[0],I[1]+1): 
				for v in V:
					coverage_set.add((v,t))   
		return coverage_set

	def Pruning(self):
		s=datetime.datetime.now()
		Vc=self.kcore(self.sadj_list)
		D,Q,LI,d=set(),[],{},{} 
		for v in Vc:
			count=0 
			d[v]={}			
			for t in range(self.tmin,self.tmax+1):
				if t in self.tadj_list[v]:
					d[v][t]=len(Vc&set(self.tadj_list[v][t]))
					if d[v][t]>=self.k:
						count=count+1
					else:
						Q.append((v,t))
						d[v][t]=0
				if t not in self.tadj_list[v] or d[v][t]<self.k: 
					if count<self.sigma:
						for t1 in range(t-count,t):
							Q.append((v,t1))
							d[v][t1]=0
					else:
						if v in LI:
							LI[v].add((t-count,t-1))
						else:
							LI[v]={(t-count,t-1)}
					count=0
				else:
					if t == self.tmax:
						if count<self.sigma:
							for t1 in range(t-count+1,t+1):
								Q.append((v,t1))
								d[v][t1]=0
						else:
							if v in LI:
								LI[v].add((t-count+1,t))
							else:
								LI[v]={(t-count+1,t)}
		while len(Q)>0:
			(v,t)=Q.pop()
			D.add((v,t))
			for u in Vc&set(self.tadj_list[v][t]):
				if d[u][t]>=self.k:  
					d[u][t]=d[u][t]-1
					if d[u][t]<self.k:
						Q.append((u,t))  
						d[u][t]=0
						self.UpdateLasting(LI,u,t,Q,d)			
		tmin,tmax,tadj_list = 1000000000000,-1,{}
		with open(self.path, 'r') as file: 
			lines=file.readlines()
			for line in lines:
				line=line.split()
				from_id, to_id, time_id = int(line[0]), int(line[1]), int(line[2])
				if from_id == to_id:
					continue
				if from_id in Vc and to_id in Vc:
					if (from_id,time_id) not in D and (to_id,time_id) not in D:
						if time_id>tmax:
							tmax=time_id
						if time_id < tmin:
							tmin= time_id
						for (f_id, t_id) in [(from_id, to_id),(to_id, from_id)]:
							if f_id in tadj_list:
								if time_id in tadj_list[f_id]:
									if t_id not in tadj_list[f_id][time_id]:
										tadj_list[f_id][time_id].append(t_id)
								else:
									tadj_list[f_id][time_id] = [t_id]
							else:
								tadj_list[f_id] = {}
								tadj_list[f_id][time_id] = [t_id]														  
		return [tadj_list,tmin,tmax,LI] 

	def UpdateLasting(self,LI,u,t,Q,d):
		for (s,e) in LI[u]:
			if s<=t<=e:
				LI[u].remove((s,e))
				if t-s>=self.sigma:
					LI[u].add((s,t-1))
				else:
					for t1 in range(s,t):
						Q.append((u,t1))
						d[u][t1]=0
				if e-t>=self.sigma:
					LI[u].add((t+1,e))
				else:
					for t2 in range(t+1,e+1):
						Q.append((u,t2))
						d[u][t2]=0
				return  
	def TopLC(self,k,sigma,r):
		self.k, self.sigma, self.r= k, sigma,r
		tadj_list1,tmin1,tmax1,LI=self.Pruning()
		print(self.path+"toplc")		
		starttime=datetime.datetime.now()   
		self.R,self.A,self.B,self.flag=[],{},{},[]   
		search_count,intersection_graph_count=[0],[0]
		for timestamp in range(tmin1,tmax1+1-self.sigma+1):
			interval=(timestamp,timestamp+self.sigma-1)
			P=set()
			for v in tadj_list1:
				P.add(v)
			self.search(interval,LI,P,tadj_list1,tmax1,search_count,intersection_graph_count)
		endtime = datetime.datetime.now()
		print("search_count:"+str(search_count))
		print("intersection_graph_count"+str(intersection_graph_count))
		print(self.R)
		print('search_time (s): ' + str(endtime - starttime)+"coverage"+str(len(self.A)))

	def search(self,interval,LI,S,tadj_list1,tmax1,search_count,intersection_graph_count): 
		search_count[0]=search_count[0]+1		
		ts, te = interval[0], interval[1]
		D=set()
		for u in S:
			index=0
			for (t1,t2) in LI[u]:
				if t1<=ts and te<=t2:
					break
				else:
					index=index+1
			if index==len(LI[u]):
				D.add(u)
		S.difference_update(D)
		if len(self.R)==self.r and len(S)*(tmax1-ts+1)<min(self.B.keys())+len(self.A)/self.r:	
			return
		temp=self.intersection_graph(tadj_list1,interval,S)
		intersection_graph_count[0]=intersection_graph_count[0]+1
		if len(temp[1])==0:
			return
		if len(self.flag)==0:
			self.flag.append(temp)
		else:
			H=self.flag.pop()
			self.flag.append(temp)
			if  H[0][0]==ts and H[1]!=temp[1]:
				for result in self.R:
					if set(H[1]).issubset(set(result[1])) and result[0][0]<=H[0][0] and result[0][1]>=H[0][1]:
						return
				self.Update(H)
			if H[0][0]!=ts:
				self.Update(H)
		if te==tmax1:
			return
		else:
			self.search((ts,te+1),LI,S,tadj_list1,tmax1,search_count,intersection_graph_count)
	def Update(self,temp): 
		if len(self.R)<self.r:
			self.add(temp)
		else:
			if self.pcov(temp)>=min(self.B.keys())+len(self.A)/self.r:
				self.delete()
				self.add(temp)
	
	def pcov(self,temp):
		count=0
		R_min=self.B[min(self.B.keys())][0]
		for v in temp[1]:
			for t in range(temp[0][0],temp[0][1]+1):
				if (v,t) not in self.A:
					count=count+1
					continue
				if v in R_min[1] and R_min[0][0]<=t<=R_min[0][1] and len(self.A[(v,t)])==1:
					count=count+1
		return count
			
	def add(self,temp):
		self.R.append(temp)
		delta=0
		for v in temp[1]:
			for t in range(temp[0][0],temp[0][1]+1):
				if (v,t) not in self.A:
					self.A[(v,t)]=[temp]
					delta=delta+1
					continue
				if len(self.A[(v,t)])==1:
					R=self.A[(v,t)][0]
					for i in self.B:
						if R in self.B[i]:
							self.B[i].remove(R)
							if len(self.B[i])==0:
								del self.B[i]
							if i-1 in self.B:							   
							   self.B[i-1].append(R)
							else:
								self.B[i-1]=[R]
							break
				self.A[(v,t)].append(temp)
		if delta not in self.B:
			self.B[delta]=[temp]
		else:
			self.B[delta].append(temp)
		
	def delete(self):
		min_pcov=min(self.B.keys())
		R_min=self.B[min_pcov].pop()
		self.R.remove(R_min)
		if len(self.B[min_pcov])==0:
			del self.B[min_pcov]
		for v in R_min[1]:
			for t in range(R_min[0][0],R_min[0][1]+1):
				self.A[(v,t)].remove(R_min)
				if len(self.A[(v,t)])==1:
					R=self.A[(v,t)][0]
					for i in self.B:
						if R in self.B[i]:
							self.B[i].remove(R)
							if len(self.B[i])==0:
								del self.B[i]
							if i+1 in self.B:
								self.B[i+1].append(R)
							else:
								self.B[i+1]=[R]
							break
					continue
				if len(self.A[(v,t)])==0:
					del self.A[(v,t)]  

	def TopLC_B(self,k,sigma,r):
		self.k, self.sigma, self.r= k, sigma,r
		print(self.path+"toplc_b")	   
		tadj_list1,tmin1,tmax1,LI=self.Pruning()
		starttime=datetime.datetime.now()
		self.R,self.flag=[],[]   
		search_count,intersection_graph_count=[0],[0]
		for timestamp in range(tmin1,tmax1+1-self.sigma+1):
			interval=(timestamp,timestamp+self.sigma-1)
			P=set()
			for v in tadj_list1:
				P.add(v)
			self.search_b(interval,LI,P,tadj_list1,tmax1,search_count,intersection_graph_count)
		endtime = datetime.datetime.now()
		print("search_count:"+str(search_count))
		print("intersection_graph_count"+str(intersection_graph_count))
		print("R: "+str(len(self.R)))
		print('time (s): ' + str(endtime - starttime)+"coverage"+str(len(self.coverage_measure(self.R))))

	def search_b(self,interval,LI,S,tadj_list1,tmax1,search_count,intersection_graph_count): 
		search_count[0]=search_count[0]+1		
		ts, te = interval[0], interval[1]
		D=set()
		for u in S:
			index=0
			for (t1,t2) in LI[u]:
				if t1<=ts and te<=t2:
					break
				else:
					index=index+1
			if index==len(LI[u]):
				D.add(u)
		S.difference_update(D)
		if te==tmax1:
			self.flag=[]
			return
		if len(self.R)==self.r:
			cov_R=len(self.coverage_measure(self.R))
			min_size=10000000000
			for result in self.R:
				tem=self.R[:]
				tem.remove(result)
				size=len(self.coverage_measure([result])-self.coverage_measure(tem))
				if size<min_size:
					min_size=size		
			if len(S)*(tmax1-ts+1)<min_size+cov_R/self.r:	
				self.flag=[]
				return
		temp=self.intersection_graph(tadj_list1,interval,S)
		intersection_graph_count[0]=intersection_graph_count[0]+1
		if len(temp[1])==0:
			self.flag=[]
			return
		if self.flag==[]:
			self.flag.append(temp)
		else:
			H=self.flag.pop()
			self.flag.append(temp)
			if H[1]!=temp[1]:
				for result0 in self.R:
					if set(H[1]).issubset(set(result0[1])) and result0[0][0]<=H[0][0] and result0[0][1]>=H[0][1]:
						return
				self.Update_b(H)
		self.search_b((ts,te+1),LI,S,tadj_list1,tmax1,search_count,intersection_graph_count)
	def Update_b(self,temp):
		if len(self.R)<self.r:
			self.R.append(temp)
		else:
			cov_R1=len(self.coverage_measure(self.R))
			min_size1=10000000000
			for result1 in self.R:
				tem1=self.R[:]
				tem1.remove(result1)
				size1=len(self.coverage_measure([result1])-self.coverage_measure(tem1))
				if size1<min_size1:
					min_size1=size1
					min_r=result1
			self.R.remove(min_r)
			self.R.append(temp)
			if len(self.coverage_measure(self.R))<(1+1/self.r)*cov_R1:
				self.R.remove(temp)
				self.R.append(min_r)
				
	def metric(self,R):
		AB,ATC=0,0
		for res in R:
			temp=0
			for t in range(res[0][0],res[0][1]+1):
				for v in res[1]:
					if t in self.tadj_list[v]:
						temp=temp+len(set(self.tadj_list[v][t])&res[1])
			AB=AB+temp/(len(res[1])*(len(res[1])-1)*(res[0][1]-res[0][0]+1))
		AB=AB/len(R)
		for res in R:
			cut,vol,vol_bar=0,0,0
			for t in range(res[0][0],res[0][1]+1):
				for u in res[1]:
					for v in set(self.sadj_list.keys())-set(res[1]):
						if v in self.tadj_list[u][t]:
							cut=cut+1
					vol=vol+len(self.tadj_list[u][t])
				for w in set(self.sadj_list.keys())-set(res[1]):
					if t in self.tadj_list[w]:
						vol_bar=vol_bar+len(self.tadj_list[w][t])
			if vol>vol_bar:
				vol=vol_bar
			ATC=ATC+cut/vol
		ATC=ATC/len(R)
		print('AB '+str(AB)+" ATC "+str(ATC))

if __name__ == '__main__':
	G=Graph("example.txt")
	k,sigma,r=2,2,3
	G.TopLC(k,sigma,r)
	'''
	path2="enron_month"
	path1="lkml_month"
	path_all=[path1,path2]
	for path in path_all:
		G=Graph(path)
		sigma,r=3,10 
		for k in range(3,8):
			print("k, sigma, r"+str(k)+str(sigma)+str(r))
			G.Naive(k,sigma)
			starttime = datetime.datetime.now()
			print(path+"enum")
			G.Enum(k,sigma)
			endtime = datetime.datetime.now()
			print('time (s): ' + str(endtime - starttime))
			G.GreLC(k,sigma,r)
			G.TopLC_B(k,sigma,r)
			G.TopLC(k,sigma,r)
		k,r=4,10
		for sigma in range(2,7):
			print("k, sigma, r"+str(k)+str(sigma)+str(r))
			G.Naive(k,sigma)
			starttime = datetime.datetime.now()
			print(path+"enum")
			G.Enum(k,sigma)
			endtime = datetime.datetime.now()
			print('time (s): ' + str(endtime - starttime))
			G.GreLC(k,sigma,r)
			G.TopLC_B(k,sigma,r)
			G.TopLC(k,sigma,r)

		k,sigma=4,3
		for r in range(5,30,5):
			print("k, sigma, r"+str(k)+str(sigma)+str(r))
			G.Naive(k,sigma)
			starttime = datetime.datetime.now()
			print(path+"enum")
			G.Enum(k,sigma)
			endtime = datetime.datetime.now()
			print('time (s): ' + str(endtime - starttime))
			G.GreLC(k,sigma,r)
			G.TopLC_B(k,sigma,r)
	'''
   
		
