def pre(path):
    tadj_list={}
    with open(path, 'r') as file: 
        lines=file.readlines()
        for line in lines:
            line = line.split()
            from_id, to_id, time_id = int(line[0]), int(line[1]), int(line[2])
            if from_id == to_id:
                continue
            for (f_id, t_id) in [(from_id, to_id)]:
                if f_id in tadj_list:
                    if t_id in tadj_list[f_id]:
                        if time_id not in tadj_list[f_id][t_id]:
                            tadj_list[f_id][t_id].append(time_id)
                    else:
                        tadj_list[f_id][t_id] = [time_id]
                else:
                    tadj_list[f_id] = {}
                    tadj_list[f_id][t_id] = [time_id]
    res=[]
    for from_id in tadj_list:
        for to_id in tadj_list[from_id]:
            time_list=sorted(tadj_list[from_id][to_id])
            start=time_list.pop(0)
            res.append((start,from_id,to_id,1))
            for time in time_list:
                if time!=start+1:
                    res.append((start+1,from_id,to_id,0))
                    res.append((time,from_id,to_id,1))
                    start=time
                else:
                    start=time
    res.sort()
    new_file=open(path+"new",'w')
    for temp in res:
        new_file.write(str(temp[0])+'\t'+str(temp[1])+'\t'+str(temp[2])+'\t'+str(temp[3])+'\n')
    new_file.close()

if __name__=="__main__":
    pre("author_relationcase")

    


