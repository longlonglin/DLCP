#include<map>
#include<set>
#include<ctime>
#include<cstdio>
#include<vector>
#include<cstring>
#include<algorithm>
using namespace std;

const double eps=1e-15;
const int inf=2147483647;
double r;
int lv,K,m_s,m_t,m_rt,stop;
char f_name[96],o_name[96];
map<int,vector<pair<int,int> > >tc;
time_t st_time;

bool cmp(pair<int,int>a,pair<int,int>b)
{
	if(a.first==b.first)return a.second>b.second;
	return a.first<b.first;
}

void deg_op(vector<pair<int,int> >&c,vector<pair<int,int> >b,int op)
{
	if(b.size()&&!b[0].second)
		b.erase(b.begin());
	if(op!=2)
	{
		if(!b.size())
		{
			if(!op)c.clear();
			return;
		}
		if(op==3)
		{
			int s=b.size();
			for(int i=0;i<s;i++)
				b[i].second=-b[i].second;
			op=1;
		}
		if(!c.size())
		{
			if(op==1)c=b;
			b.clear();
			return;
		}
	}
	int sa=c.size(),sb=b.size(),i,j,da=0,db=0,t,d0=0,d1;
	vector<pair<int,int> >a=c;
	c.clear();
	for(i=j=0;i<sa||j<sb;)
	{
		if(i<sa&&(j>=sb||a[i].first<b[j].first))
		{
			da=a[i].second;
			t=a[i++].first;
		}
		else
		{
			if(i<sa&&a[i].first==b[j].first)da=a[i].second;
			db=b[j].second;
			t=b[j++].first;
		}
		d1=op?op>1?da>db?db:da:da+db:da*db;
		if(d1!=d0)
		{
			c.push_back(make_pair(t,d1));
			d0=d1;
		}
	}
	a.clear();
	b.clear();
}

vector<pair<int,int> >deg_shift(vector<pair<int,int> >&d,int t)
{
	vector<pair<int,int> >r;
	for(int i=0,s=d.size();i<s;i++)
		r.push_back(make_pair(d[i].first+t,d[i].second));
	return r;
}

void int_op(vector<pair<int,int> >&a,vector<pair<int,int> >b,int op)
{
	vector<pair<int,int> >ca,cb,cab;
	int sa=a.size(),sb=b.size(),i,j,k=0,l;
	for(i=0;i<sa;i++)
	{
		ca.push_back(make_pair(a[i].first,1));
		ca.push_back(make_pair(a[i].second,-1));
	}
	a.clear();
	for(i=0;i<sb;i++)
	{
		cb.push_back(make_pair(b[i].first,1));
		cb.push_back(make_pair(b[i].second,-1));
	}
	b.clear();
	sa<<=1;
	sb<<=1;
	for(i=j=0;i<sa||j<sb;)
		if(i<sa&&(j>=sb||ca[i].first<cb[j].first||ca[i].first==cb[j].first&&(op&&ca[i].second<cb[j].second||!op&&ca[i].second>cb[j].second)))cab.push_back(ca[i++]);
		else cab.push_back(cb[j++]);
	ca.clear();
	cb.clear();
	sa+=sb;
	for(i=j=0;i<sa;i++)
	{
		k+=cab[i].second;
		if(k>op)
			if(j<=op)l=cab[i].first;
			else;
		else
			if(j>op)a.push_back(make_pair(l,cab[i].first));
		j=k;
	}
	cab.clear();
}

vector<pair<int,int> >deg_set(vector<pair<int,int> >&deg,int cmpv,int d1,int d2)
{
	vector<pair<int,int> >res;
	int s=deg.size(),i,j,d0=0,d;
	for(j=i=0;i<s;i++)
	{
		d=i<s-1?deg[i].second<cmpv?d2:d1:0;
		if(d!=d0)
		{
			res.push_back(make_pair(deg[i].first,d));
			d0=d;
		}
	}
	return res;
}

vector<pair<int,int> >l_to_d(vector<pair<int,int> >link,int d)
{
	vector<pair<int,int> >deg;
	int s=link.size(),i;
	for(i=0;i<s;i++)
	{
		deg.push_back(make_pair(link[i].first,d));
		deg.push_back(make_pair(link[i].second,0));
	}
	link.clear();
	return deg;
}

vector<pair<int,int> >d_to_l(vector<pair<int,int> >deg,int mt)
{
	vector<pair<int,int> >l;
	int s=deg.size(),i,j=0;
	for(i=0;i<s;i++)
	{
		if(deg[i].second>0)deg[i].second=1;
		else deg[i].second=0;
		if(deg[i].second==j)deg[i].first=-1;
		else j=deg[i].second;
	}
	for(i=j=0;i<s;i++)
		if(deg[i].first>=0)deg[j++]=deg[i];
	for(i=0;i<j;i+=2)
	{
		int t1=deg[i].first,t2=deg[i+1].first;
		if(t2-t1>=mt)l.push_back(make_pair(t1,t2));
	}
	deg.clear();
	return l;
}

void prune_ub(vector<pair<int,int> >&deg)
{
	int i,j=0,s=deg.size(),l,m,t;
	vector<pair<int,int> >changes;
	changes.push_back(make_pair(0,inf));
	for(i=0;i<s;i++)
	{
		l=deg[i].second-j;
		t=deg[i].first;
		for(m=changes.size()-1;l<0;m--)
			if(changes[m].second>0&&t-changes[m].first<m_t)
				if(l+changes[m].second<=0)
				{
					l+=changes[m].second;
					changes.resize(m);
				}
				else
				{
					changes[m].second+=l;
					l=0;
				}
			else break;
		if(l)changes.push_back(make_pair(t,l));
		j=deg[i].second;
	}
	deg.clear();
	s=changes.size();
	l=changes[0].second-inf;
	if(l)deg.push_back(make_pair(0,l));
	for(i=1;i<s;i++)
	{
		l+=changes[i].second;
		deg.push_back(make_pair(changes[i].first,l));
	}
}

struct triple
{
	int t,d0,d1;
};

bool tcmp(triple a,triple b)
{
	return a.t<b.t;
}

int pos(int *list,int s,int x,int z)
{
	int l=-1,r=s,m;
	if(z<0)
	{
		r++;
		while(1)
		{
			m=(l+r)/2;
			if((m>=s||list[m]<=x)&&(!m||list[m-1]>x))return m;
			if(m<0||m<s&&list[m]>x)l=m;
			else r=m;
		}
	}
	else
	{
		l--;
		while(1)
		{
			m=(l+r)/2;
			if((m<0||list[m]>=x)&&(m+2>s||list[m+1]<x))return m;
			if(m>=0&&(m>=s||list[m]<x))r=m;
			else l=m;
		}
	}
}

void t_sort(vector<vector<pair<int,int> > >&degs)
{
	vector<triple>changes;
	triple change;
	int i,j,k,l,ds,s,t,z,*list;
	ds=degs.size();
	for(i=0;i<ds;i++)
	{
		s=degs[i].size();
		for(j=0;j<s;j++)
		{
			change.t=degs[i][j].first;
			change.d0=j?degs[i][j-1].second:0;
			change.d1=degs[i][j].second;
			changes.push_back(change);
		}
		degs[i].clear();
	}
	sort(changes.begin(),changes.end(),tcmp);
	list=new int[ds];
	memset(list,0,ds<<2);
	s=changes.size();
	for(i=0;i<s;i++)
	{
		t=changes[i].t;
		j=changes[i].d0;
		k=changes[i].d1;
		z=-1;
		if(j>k)z=1;
		l=pos(list,ds,k,-z)-z;
		for(j=pos(list,ds,j,z);j!=l;j+=z)
			if(list[j]!=list[j+z])
			{
				list[j]=list[j+z];
				degs[j].push_back(make_pair(t,list[j]));
			}
		degs[l].push_back(make_pair(t,k));
		list[l]=k;
	}
	changes.clear();
	delete[]list;
	for(i=0;i<ds;i++)
	{
		s=degs[i].size();
		for(j=k=0;j<s;j++)
			if(degs[i][k].first==degs[i][j].first)degs[i][k]=degs[i][j];
			else degs[i][++k]=degs[i][j];
		degs[i].resize(k+1);
		for(j=k++;j;j--)
			if(degs[i][j].second==degs[i][j-1].second)degs[i][j].first=-1;
		for(j=s=0;j<k;j++)
			if(degs[i][j].first>=0)degs[i][s++]=degs[i][j];
		degs[i].resize(s);
	}
}

long long get_Tdeg(vector<pair<int,int> >&deg,int d)
{
	long long ds=0;
	for(int k=d,s=deg.size();k<s;k++)
		if(d)ds+=(long long)(deg[k].first-deg[k-1].first)*deg[k-1].second;
		else ds+=deg[k].second-deg[k].first;
	return ds;
}

struct vertex
{
	int s;
	map<int,vector<pair<int,int> > >links;
	vector<pair<int,int> >deg,adeg,cdeg,acdeg,ub;

	vertex()
	{
		s=0;
	}

	void get_deg(set<int>*c)
	{
		map<int,vector<pair<int,int> > >::iterator j;
		vector<pair<int,int> >changes;
		int s,k,t,t0,t1,d0,d1;
		if(c==NULL)
		{
			deg.clear();
			adeg.clear();
		}
		else cdeg.clear();
		for(j=links.begin();j!=links.end();j++)
		{
			if(c!=NULL&&c->find(j->first)!=c->end())continue;
			s=j->second.size();
			for(k=0;k<s;k++)
			{
				changes.push_back(make_pair(j->second[k].first,1));
				changes.push_back(make_pair(j->second[k].second,-1));
			}
		}
		sort(changes.begin(),changes.end());
		s=changes.size();
		t=t0=t1=0;
		d0=d1=0;
		for(k=0;k<s;k++)
		{
			t=changes[k].first;
			if(t>t1)
			{
				if(d1!=d0)
				{
					if(c==NULL)adeg.push_back(make_pair(t1,d1));
					else cdeg.push_back(make_pair(t1,d1));
					d0=d1;
					t0=t1;
				}
				t1=t;
			}
			d1+=changes[k].second;
		}
		if(d1!=d0)
			if(c==NULL)adeg.push_back(make_pair(t1,d1));
			else cdeg.push_back(make_pair(t1,d1));
		changes.clear();
	}
};

vector<pair<pair<int,int>,vector<int> > >topK;
map<int,vector<pair<int,int> > >cov,covr;
long long minS,covS;
int mini=-1;

long long priv(map<int,vector<pair<int,int> > >&a,vector<pair<int,int> >b,int v)
{
	if(a.find(v)==a.end())return get_Tdeg(b,0);
	vector<pair<int,int> >c=a[v];
	int_op(b,c,0);
	return get_Tdeg(b,0)-get_Tdeg(c,0);
}

long long priS(map<int,vector<pair<int,int> > >&a,pair<pair<int,int>,vector<int> >&b)
{
	long long ds=0;
	vector<pair<int,int> >l;
	l.push_back(b.first);
	for(int i=0,s=b.second.size();i<s;i++)
		ds+=priv(a,l,b.second[i]);
	return ds;
}

void add_cov(map<int,vector<pair<int,int> > >&a,pair<pair<int,int>,vector<int> >&b)
{
	vector<pair<int,int> >l;
	l.push_back(b.first);
	for(int i=0,s=b.second.size();i<s;i++)
		int_op(a[b.second[i]],l,0);
}

void updateK(pair<pair<int,int>,vector<int> >rec)
{
	int i,j,s=topK.size();
	long long ds=covS+priS(covr,rec);
	if(s<K)
	{
		topK.push_back(rec);
		s++;
	}
	else
		if(ds<=covS+minS/*+(covS+minS)/K*/)return;
		else topK[mini]=rec;
	printf(":%d %lld %lu %d %d %d\n",s,ds-covS-minS,rec.second.size(),rec.first.second-rec.first.first,rec.first.first,rec.first.second);
	FILE *f=fopen(o_name,"a");
	fprintf(f," e=%d, s=%d, V={",rec.first.second,rec.first.first);
	for(int k=0,t=rec.second.size();k<t;k++)
	{
		if(k)fprintf(f,",");
		fprintf(f,"%d",rec.second[k]);
	}
	fprintf(f,"}\n");
	fclose(f);
	for(i=0;i<lv;i++)
		printf(" ");
	stop=0;
	if(s<K)return;
	minS=-1;
	for(i=0;i<K;i++)
	{
		covr.clear();
		for(j=0;j<K;j++)
			if(j!=i)add_cov(covr,topK[j]);
		ds=priS(covr,topK[i]);
		if(minS<0||ds<minS)
		{
			minS=ds;
			mini=i;
		}
	}
	covr.clear();
	for(j=0;j<K;j++)
		if(j!=mini)add_cov(covr,topK[j]);
	covS=0;
	for(map<int,vector<pair<int,int> > >::iterator k=covr.begin();k!=covr.end();k++)
		covS+=get_Tdeg(k->second,0);
}

void clv(map<int,vertex>&g,vector<pair<int,int> >ri)
{
	map<int,vertex>::iterator v;
	map<int,vector<pair<int,int> > >::iterator j;
	if(ri.size())
	{
		vector<pair<int,int> >rd=l_to_d(ri,1);
		for(v=g.begin();v!=g.end();v++)
		{
			deg_op(v->second.deg,rd,0);
			deg_op(v->second.adeg,rd,0);
			deg_op(v->second.cdeg,rd,0);
			deg_op(v->second.acdeg,rd,0);
			for(j=v->second.links.begin();j!=v->second.links.end();j++)
				int_op(j->second,ri,1);
		}
	}
	vector<int>del_v;
	for(v=g.begin();v!=g.end();v++)
	{
		vector<int>del;
		for(j=v->second.links.begin();j!=v->second.links.end();j++)
			if(j->second.empty())del.push_back(j->first);
		for(int k=0,s=del.size();k<s;k++)
			v->second.links.erase(del[k]);
		if(v->second.links.empty())del_v.push_back(v->first);
	}
	for(int k=0,s=del_v.size();k<s;k++)
		g.erase(del_v[k]);
}

struct task
{
	map<int,vertex>g;
	vector<int>R;
	int ta,tb;
	vector<pair<int,int> >U;

	void prune()
	{
		vector<pair<int,int> >remain_i,remain_d,um,lm,Um,Lm;
		pair<pair<int,int>,vector<int> >rec;
		map<int,vertex>::iterator v;
		map<int,vector<pair<int,int> > >::iterator j;
		int i,k,l,m,u,n=g.size(),S=R.size(),s,sc,sd,ld=int(r*(m_s-1))+1,t,lt,d,dc,dr,re,re0,tub=tb-ta;
		if(S>m_s)ld=int(r*(S-1))+1;
		remain_d.push_back(make_pair(ta,1));
		remain_d.push_back(make_pair(tb,0));
		do
			for(u=0,v=g.begin();v!=g.end();v++)
				if(v->second.adeg.size())
				{
					i=v->first;
					deg_op(v->second.deg,v->second.adeg,1);
					deg_op(v->second.cdeg,v->second.acdeg,1);
					v->second.adeg.clear();
					v->second.acdeg.clear();
					s=v->second.deg.size();
					sc=v->second.cdeg.size();
					sd=remain_d.size();
					for(lt=ta,re0=1,d=dc=dr=k=l=m=0;k<s||l<sc||m<sd;)
					{
						t=tb;
						if(k<s&&v->second.deg[k].first<t)t=v->second.deg[k].first;
						if(l<sc&&v->second.cdeg[l].first<t)t=v->second.cdeg[l].first;
						if(m<sd&&remain_d[m].first<t)t=remain_d[m].first;
						if(k<s&&t==v->second.deg[k].first)d=v->second.deg[k++].second;
						if(l<sc&&t==v->second.cdeg[l].first)dc=v->second.cdeg[l++].second;
						if(m<sd&&t==remain_d[m].first)dr=remain_d[m++].second;
						re=1;
						if(d>0&&(!dr||d<ld||d<r*(S-v->second.s+dc)))re=0;
						if(re0&&!re)
						{
							if(t>lt)remain_i.push_back(make_pair(lt,t));
							lt=-1;
						}
						if(!re0&&re)lt=t;
						re0=re;
					}
					if(lt>=0&&tb>lt)remain_i.push_back(make_pair(lt,tb));
					if(remain_i.size()!=1||remain_i[0].first>ta||remain_i[0].second<tb)
					{
						u=1;
						deg_op(v->second.deg,l_to_d(remain_i,1),0);
						remain_i=d_to_l(v->second.deg,m_t);
						vector<pair<int,int> >redeg=l_to_d(remain_i,1);
						if(v->second.s)remain_d=redeg;
						deg_op(v->second.deg,redeg,0);
						deg_op(v->second.cdeg,redeg,0);
						deg_op(v->second.ub,redeg,0);
						for(j=v->second.links.begin();j!=v->second.links.end();j++)
						{
							k=j->first;
							int_op(j->second,remain_i,1);
							if(j->second!=g[k].links[i])
							{
								redeg=l_to_d(g[k].links[i],-1);
								deg_op(redeg,l_to_d(j->second,1),1);
								g[k].links[i]=j->second;
								deg_op(g[k].adeg,redeg,1);
								if(!v->second.s)deg_op(g[k].acdeg,redeg,1);
							}
						}
						redeg.clear();
					}
					remain_i.clear();
				}
		while(u);
		if(S>1&&remain_d.size()==2)
		{
			vector<vector<pair<int,int> > >degs;
			for(i=0,s=R.size();i<s;i++)
				degs.push_back(g[R[i]].deg);
			for(i=S-1,k=0;i;i--,k++)
			{
				if(k>=i)k=0;
				deg_op(degs[k],degs[i],2);
				degs.resize(i);
			}
			for(l=i=0,s=degs[0].size();i<s;i++)
			{
				k=int(degs[0][i].second/r)+1-S;
				if(k<0)k=0;
				if(k>0)l=1;
				if(l)um.push_back(make_pair(degs[0][i].first,k));
			}
			degs.clear();
			deg_op(um,U,2);
			prune_ub(um);
			for(i=0,s=R.size();i<s;i++)
			{
				v=g.find(R[i]);
				lm=v->second.deg;
				deg_op(lm,v->second.cdeg,3);
				degs.push_back(lm);
			}
			lm.clear();
			for(i=S-1,k=0;i;i--,k++)
			{
				if(k>=i)k=0;
				deg_op(degs[k],degs[i],2);
				degs.resize(i);
			}
			s=degs[0].size();
			if(!s||degs[0][0].first>ta)lm.push_back(make_pair(ta,-int(r*(S-1)/(1-r)+1)));
			for(i=0;i<s;i++)
			{
				k=int((r*(S-1)-degs[0][i].second)/(1-r)+1);
				if(k<0)k=0;
				lm.push_back(make_pair(degs[0][i].first,-k));
			}
			s=lm.size()-1;
			if(lm[s].first<tb)lm.push_back(make_pair(tb,-n));
			else lm[s].second=-n;
			degs.clear();
			vector<pair<int,int> >ulm;
			int rb=m_s-S;
			if(mini>=0)
			{
				rec=make_pair(make_pair(ta,tb),R);
				long long covR=covS+priS(covr,rec),covT=covS+minS+(covS+minS)/K;
				vector<long long>pric;
				for(v=g.begin();v!=g.end();v++)
					if(!v->second.s)pric.push_back(priv(covr,d_to_l(v->second.deg,m_t),v->first));
				sort(pric.begin(),pric.end());
				s=pric.size();
				for(k=s;k&&covR<=covT;covR+=pric[--k]);
				if(s-k>rb)rb=s-k;
			}
			ulm.push_back(make_pair(ta,-rb));
			deg_op(lm,ulm,2);
			prune_ub(lm);
			ulm=lm;
			lm.clear();
			deg_op(lm,ulm,3);
			ulm.resize(2);
			ulm[0]=make_pair(ta,1);
			ulm[1]=make_pair(tb,0);
			deg_op(ulm,um,1);
			deg_op(ulm,lm,3);
			deg_op(remain_d,l_to_d(d_to_l(ulm,m_t),1),0);
			ulm.clear();
			if(remain_d.size()!=2)
			{
				Um=um;
				Lm=lm;
			}
			else
			{
				vector<pair<int,int> >sum_deg,tdeg;
				map<int,vector<pair<int,int> > >cdegs,sdegs;
				int Si;
				vector<pair<long long,int> >Tdeg;
				for(v=g.begin();v!=g.end();v++)
				{
					tdeg=v->second.deg;
					deg_op(tdeg,v->second.cdeg,3);
					if(v->second.s)
					{
						deg_op(sum_deg,tdeg,1);
						sdegs[v->first]=tdeg;
						Tdeg.push_back(make_pair(get_Tdeg(tdeg,1),v->first));
					}
					else cdegs[v->first]=tdeg;
				}
				tdeg.clear();
				sort(Tdeg.begin(),Tdeg.end());
				for(Si=S;;)
				{
					for(j=cdegs.begin();j!=cdegs.end();j++)
						degs.push_back(j->second);
					t_sort(degs);
					s=degs.size();
					degs.push_back(tdeg);
					for(i=s++;i;i--)
						degs[i]=degs[i-1];
					degs[0]=sum_deg;
					Um.push_back(make_pair(ta,1));
					Um.push_back(make_pair(tb,0));
					Lm.push_back(make_pair(ta,s));
					Lm.push_back(make_pair(tb,s+1));
					k=um.size();
					l=0;
					for(i=0;i<k;i++)
						if(um[i].second>l)l=um[i].second;
					if(++l<s)
					{
						s=l;
						degs.resize(s);
					}
					k=lm.size();
					if(!k||lm[0].first>ta)l=0;
					else
					{
						l=s;
						for(i=0;i<k;i++)
							if(lm[i].second<l)l=lm[i].second;
					}
					vector<pair<int,int> >ci;
					for(k=i=0;i<s;i++)
					{
						if(i)deg_op(degs[i],degs[i-1],1);
						if(i>=l)
						{
							ci=deg_set(degs[i],Si*(int(r*(S-1+i))+1),-i,1);
							if(ci.size()!=2||ci[0]!=make_pair(ta,1)||ci[1]!=make_pair(tb,0))
							{
								deg_op(Um,ci,2);
								deg_op(Lm,deg_set(ci,1,n,i),2);
							}
						}
					}
					ci.clear();
					degs.clear();
					tdeg=Um;
					Um.clear();
					deg_op(Um,tdeg,3);
					deg_op(Um,um,2);
					deg_op(U,Um,2);
					prune_ub(Um);
					tdeg=Lm;
					Lm.clear();
					deg_op(Lm,tdeg,3);
					tdeg.clear();
					deg_op(tdeg,lm,3);
					deg_op(Lm,tdeg,2);
					prune_ub(Lm);
					tdeg=Lm;
					Lm.clear();
					deg_op(Lm,tdeg,3);
					tdeg.clear();
					ulm.push_back(make_pair(ta,1));
					ulm.push_back(make_pair(tb,0));
					deg_op(ulm,Um,1);
					deg_op(ulm,Lm,3);
					deg_op(remain_d,l_to_d(d_to_l(ulm,m_t),1),0);
					ulm.clear();
					if(remain_d.size()!=2)break;
					for(int mt=m_t-1;mt<tb-ta;mt+=(mt+9)/10)
					{
						for(j=cdegs.begin();j!=cdegs.end();j++)
						{
							tdeg=j->second;
							deg_op(tdeg,deg_shift(tdeg,mt),1);
							degs.push_back(tdeg);
						}
						t_sort(degs);
						s=degs.size();
						degs.push_back(tdeg);
						for(i=s++;i;i--)
							degs[i]=degs[i-1];
						degs[0]=sum_deg;
						deg_op(degs[0],deg_shift(degs[0],mt),1);
						vector<pair<int,int> >ci,tUm,tLm;
						tUm.push_back(make_pair(ta,1));
						tUm.push_back(make_pair(tb+mt,0));
						tLm.push_back(make_pair(ta,s));
						tLm.push_back(make_pair(tb+mt,s+1));
						k=um.size();
						l=0;
						for(i=0;i<k;i++)
							if(um[i].second>l)l=um[i].second;
						if(++l<s)
						{
							s=l;
							degs.resize(s);
						}
						k=lm.size();
						if(!k||lm[0].first>ta)l=0;
						else
						{
							l=s;
							for(i=0;i<k;i++)
								if(lm[i].second<l)l=lm[i].second;
						}
						for(k=i=0;i<s;i++)
						{
							if(i)deg_op(degs[i],degs[i-1],1);
							if(i>=l)
							{
								ci=deg_set(degs[i],Si*(int(r*(S-1+i))+1)<<1,-i,1);
								if(ci.size()!=2||ci[0]!=make_pair(ta,1)||ci[1]!=make_pair(tb,0))
								{
									deg_op(tUm,ci,2);
									deg_op(tLm,deg_set(ci,1,n,i),2);
								}
							}
						}
						ci.clear();
						degs.clear();
						tdeg=tUm;
						tUm.clear();
						deg_op(tUm,tdeg,3);
						ulm.push_back(make_pair(ta,1));
						ulm.push_back(make_pair(tb,-n));
						deg_op(ulm,tUm,1);
						deg_op(ulm,tLm,3);
						tdeg=l_to_d(d_to_l(ulm,0),1);
						ulm.clear();
						if(tdeg.empty())
						{
							if(mt<m_t)remain_d.clear();
							if(mt<tub)tub=mt;
							break;
						}
						if(mt<m_t)
						{
							tdeg[0]=make_pair(tdeg[0].first-mt,tdeg[tdeg.size()-1].first);
							tdeg.resize(1);
							deg_op(remain_d,l_to_d(tdeg,1),0);
							k=0;
							for(i=0,s=tUm.size();i<s;i++)
								if(tUm[i].second>k)k=tUm[i].second;
							if(k)
							{
								tdeg[0]=make_pair(ta,k);
								tdeg.push_back(make_pair(tb,0));
								deg_op(Um,tdeg,2);
							}
							else Um.clear();
						}
						tdeg.clear();
						if(mini<0)break;
					}
					if(Si<3)break;
					um=Um;
					lm=Lm;
					Um.clear();
					Lm.clear();
					int bi=Tdeg[--Si].second;
					deg_op(sum_deg,sdegs[bi],3);
					sdegs.erase(bi);
					for(j=g[bi].links.begin();j!=g[bi].links.end();j++)
					{
						i=j->first;
						if(!g[i].s)deg_op(cdegs[i],l_to_d(j->second,-1),1);
					}
				}
			}
			if(remain_d.size())
			{
				for(v=g.begin();v!=g.end();v++)
					if(!v->second.adeg.size())v->second.adeg.push_back(make_pair(tb,0));
				do
					for(u=0,v=g.begin();v!=g.end();v++)
						if(v->second.adeg.size())
						{
							i=v->first;
							int sl=Lm.size(),su=Um.size(),lb=0,ub=n,o,p;
							deg_op(v->second.deg,v->second.adeg,1);
							deg_op(v->second.cdeg,v->second.acdeg,1);
							v->second.adeg.clear();
							v->second.acdeg.clear();
							s=v->second.deg.size();
							sc=v->second.cdeg.size();
							sd=remain_d.size();
							for(lt=ta,re0=1,d=dc=dr=k=l=m=o=p=0;k<s||l<sc||m<sl||o<su||p<sd;)
							{
								t=tb;
								if(k<s&&v->second.deg[k].first<t)t=v->second.deg[k].first;
								if(l<sc&&v->second.cdeg[l].first<t)t=v->second.cdeg[l].first;
								if(m<sl&&Lm[m].first<t)t=Lm[m].first;
								if(o<su&&Um[o].first<t)t=Um[o].first;
								if(p<sd&&remain_d[p].first<t)t=remain_d[p].first;
								if(k<s&&t==v->second.deg[k].first)d=v->second.deg[k++].second;
								if(l<sc&&t==v->second.cdeg[l].first)dc=v->second.cdeg[l++].second;
								if(m<sl&&t==Lm[m].first)lb=Lm[m++].second;
								if(o<su&&t==Um[o].first)ub=Um[o++].second;
								if(p<sd&&t==remain_d[p].first)dr=remain_d[p++].second;
								re=1;
								if(d>0&&(!dr||d<r*(S+lb-1)||d-dc+ub-1+v->second.s<r*(S+ub-1)))re=0;
								if(re0&&!re)
								{
									if(t>lt)remain_i.push_back(make_pair(lt,t));
									lt=-1;
								}
								if(!re0&&re)lt=t;
								re0=re;
							}
							if(lt>=0&&tb>lt)remain_i.push_back(make_pair(lt,tb));
							if(remain_i.size()!=1||remain_i[0].first>ta||remain_i[0].second<tb)
							{
								u=1;
								deg_op(v->second.deg,l_to_d(remain_i,1),0);
								remain_i=d_to_l(v->second.deg,m_t);
								vector<pair<int,int> >redeg=l_to_d(remain_i,1);
								if(v->second.s)remain_d=redeg;
								deg_op(v->second.deg,redeg,0);
								deg_op(v->second.cdeg,redeg,0);
								deg_op(v->second.ub,redeg,0);
								for(j=v->second.links.begin();j!=v->second.links.end();j++)
								{
									k=j->first;
									int_op(j->second,remain_i,1);
									if(j->second!=g[k].links[i])
									{
										redeg=l_to_d(g[k].links[i],-1);
										deg_op(redeg,l_to_d(j->second,1),1);
										g[k].links[i]=j->second;
										deg_op(g[k].adeg,redeg,1);
										if(!v->second.s)deg_op(g[k].acdeg,redeg,1);
									}
								}
								redeg.clear();
							}
							remain_i.clear();
						}
				while(u);
			}
			deg_op(U,Um,2);
			um.clear();
			lm.clear();
			Um.clear();
			Lm.clear();
		}
		s=remain_d.size();
		if(!s)
		{
			g.clear();
			return;
		}
		if(remain_d.size()!=2||remain_d[0].first>ta||remain_d[1].first<tb)
		{
			deg_op(U,remain_d,0);
			remain_i=d_to_l(remain_d,m_t);
			for(v=g.begin();v!=g.end();v++)
			{
				deg_op(v->second.deg,remain_d,0);
				deg_op(v->second.cdeg,remain_d,0);
				deg_op(v->second.ub,remain_d,0);
				for(j=v->second.links.begin();j!=v->second.links.end();j++)
					int_op(j->second,remain_i,1);
			}
			remain_i.clear();
			ta=remain_d[0].first;
			tb=remain_d[s-1].first;
			if(tb-ta<tub)tub=tb-ta;
		}
		remain_d.clear();
		vector<int>del_v;
		for(v=g.begin();v!=g.end();v++)
		{
			vector<int>del;
			for(j=v->second.links.begin();j!=v->second.links.end();j++)
				if(!j->second.size())del.push_back(j->first);
			s=del.size();
			for(k=0;k<s;k++)
				v->second.links.erase(del[k]);
			del.clear();
			if(v->second.links.empty())del_v.push_back(v->first);
		}
		for(i=0,s=del_v.size();i<s;i++)
			g.erase(del_v[i]);
		del_v.clear();
		if(int(g.size())<m_s)
		{
			g.clear();
			return;
		}
		if(mini>=0)
		{
			int tta;
			long long covR;
			for(tta=ta;;tta+=tb-tub-ta)
			{
				int ttb=tta+tub;
				vector<pair<int,int> >ri;
				ri.push_back(make_pair(tta,ttb));
				rec=make_pair(ri[0],R);
				covR=covS+priS(covr,rec);
				if(U.size())
				{
					k=0;
					for(i=0,s=U.size();i<s;i++)
						if(U[i].second>k)k=U[i].second;
					vector<long long>pric;
					for(v=g.begin();v!=g.end();v++)
					{
						vector<pair<int,int> >l=d_to_l(v->second.deg,m_t);
						int_op(l,ri,1);
						if(!v->second.s)pric.push_back(priv(covr,l,v->first));
					}
					sort(pric.begin(),pric.end());
					s=pric.size();
					if(s<k)k=s;
					for(k=s-k;k<s;k++)
						covR+=pric[k];
				}
				if(covR>covS+minS+(covS+minS)/K||tta>=tb-tub)break;
			}
			if(covR<=covS+minS+(covS+minS)/K)
			{
				g.clear();
				return;
			}
		}
	}

	int ck_deg(vertex v,int d_min)
	{
		int i,d,t,t0,s,b,mid;
		s=v.deg.size();
		d=0;
		t0=ta;
		b=-1;
		mid=ta+(tb-ta)/2;
		for(i=0;;i++)
		{
			if(i<s)t=v.deg[i].first;
			else t=tb;
			if(t>t0)
			{
				if(d<d_min)
					if(t<=mid)b=t;
					else
						if(t0<=mid)return mid;
						else
							if(t0-mid<=mid-b)return t0;
							else return b;
				t0=t;
			}
			if(i>=s)break;
			d=v.deg[i].second;
		}
		return b;
	}

	bool is_qc()
	{
		vector<int>S;
		int j,n=g.size(),d=int((n-1)*r+1);
		if(n<m_s)return false;
		for(map<int,vertex>::iterator v=g.begin();v!=g.end();v++)
		{
			j=ck_deg(v->second,d);
			if(j>0)return false;
			S.push_back(v->first);
		}
		updateK(make_pair(make_pair(ta,tb),S));
		return true;
	}

	int bk_point()
	{
		int j,bj=-1,mid=ta+(tb-ta)/2,s=R.size(),n=g.size(),d;
		if(s<m_s)s=m_s;
		d=int((s-1)*r+1);
		for(map<int,vertex>::iterator v=g.begin();v!=g.end();v++)
			if(v->second.s)
			{
				j=ck_deg(v->second,d);
				if(j>0&&labs(j-mid)<labs(bj-mid))bj=j;
			}
		return bj;
	}

	int sel_v()
	{
		int st=-1;
		vector<pair<int,int> >sdeg;
		double bds=9e99;
		for(map<int,vertex>::iterator v=g.begin();v!=g.end();v++)
			if(!v->second.s)
			{
				double ds=(R.size()+1.0)*get_Tdeg(v->second.deg,1)-(R.size()+0.0)*get_Tdeg(v->second.cdeg,1);
//				if(mini>=0)ds+=1.0*priv(covr,d_to_l(v->second.deg,m_t),v->first);
				if(ds<bds)
				{
					bds=ds;
					st=v->first;
				}
			}
		return st;
	}

	void div_t(task &h,int t)
	{
		h.g.clear();
		h.ta=ta;
		h.tb=t;
		if(t-ta<m_t&&tb-t<m_t)
		{
			g.clear();
			return;
		}
		h.R=R;
		h.U=U;
		vector<pair<int,int> >gd,hd,gl,hl;
		int i,k,s;
		gl.push_back(make_pair(t,tb));
		hl.push_back(make_pair(ta,t));
		gd=l_to_d(gl,1);
		hd=l_to_d(hl,1);
		deg_op(U,gd,0);
		deg_op(h.U,hd,0);
		h.g=g;
		vector<int>del_g,del_h;
		for(map<int,vertex>::iterator v=g.begin();v!=g.end();v++)
		{
			i=v->first;
			deg_op(v->second.deg,gd,0);
			deg_op(v->second.cdeg,gd,0);
			deg_op(v->second.ub,gd,0);
			deg_op(h.g[i].deg,hd,0);
			deg_op(h.g[i].cdeg,hd,0);
			deg_op(h.g[i].ub,hd,0);
			map<int,vector<pair<int,int> > >::iterator j;
			vector<int>del;
			for(j=v->second.links.begin();j!=v->second.links.end();j++)
			{
				k=j->first;
				int_op(h.g[i].links[k],hl,1);
				if(!h.g[i].links[k].size())h.g[i].links.erase(k);
				int_op(j->second,gl,1);
				if(!j->second.size())del.push_back(k);
			}
			s=del.size();
			for(k=0;k<s;k++)
				v->second.links.erase(del[k]);
			del.clear();
			if(v->second.links.empty())del_g.push_back(i);
			if(h.g[i].links.empty())del_h.push_back(i);
		}
		for(i=0,s=del_g.size();i<s;i++)
			g.erase(del_g[i]);
		for(i=0,s=del_h.size();i<s;i++)
			h.g.erase(del_h[i]);
		if(t-ta<m_t)h.g.clear();
		ta=t;
		if(tb-t<m_t)g.clear();
	}

	void div_v(task &h,int v)
	{
		int l,S=R.size();
		vector<pair<int,int> >u,link;
		map<int,vector<pair<int,int> > >::iterator j,k;
		map<int,vertex>::iterator i;
		set<int>c;
		h.g.clear();
		if(g[v].deg.empty())return;
		h.ta=g[v].deg[0].first;
		h.tb=g[v].deg[g[v].deg.size()-1].first;
		h.R=R;
		h.R.push_back(v);
		h.U=U;
		u.push_back(make_pair(ta,-1));
		u.push_back(make_pair(tb,0));
		deg_op(h.U,u,1);
		u[0]=make_pair(ta,-S-1);
		u[1]=make_pair(tb,0);
		deg_op(u,g[v].ub,1);
		deg_op(h.U,u,2);
		u.resize(2);
		u[0]=make_pair(h.ta,1);
		u[1]=make_pair(h.tb,0);
		deg_op(h.U,u,0);
		prune_ub(h.U);
		if(S>=3)
		{
			h.g=g;
			vector<pair<int,int> >hl;
			hl.push_back(make_pair(h.ta,h.tb));
			for(i=h.g.begin();i!=h.g.end();i++)
				for(j=i->second.links.begin();j!=i->second.links.end();j++)
					int_op(j->second,hl,1);
		}
		map<int,vector<pair<int,int> > >t_2hop;
		map<int,vector<pair<int,int> > >::iterator t;
		for(j=g[v].links.begin();j!=g[v].links.end();j++)
		{
			int l=j->first;
			if(S<3)
			{
				int_op(t_2hop[l],j->second,0);
				for(k=g[l].links.begin();k!=g[l].links.end();k++)
				{
					link=j->second;
					int_op(link,k->second,1);
					int_op(t_2hop[k->first],link,0);
				}
			}
			link=l_to_d(j->second,-1);
			deg_op(g[l].adeg,link,1);
			deg_op(g[l].acdeg,link,1);
		}
		if(S<3)
		{
			for(t=t_2hop.begin();t!=t_2hop.end();t++)
				if(t->second.size())t->second=d_to_l(l_to_d(t->second,1),m_t);
			for(t=t_2hop.begin();t!=t_2hop.end();t++)
				if(t->second.size())
				{
					int o=t->first;
					for(j=g[o].links.begin();j!=g[o].links.end();j++)
					{
						l=j->first;
						if(l<o||!t_2hop[l].size())continue;
						link=t_2hop[o];
						int_op(link,t_2hop[l],1);
						if(!link.size())continue;
						int_op(link,j->second,1);
						if(!link.size())continue;
						h.g[o].links[l]=link;
						h.g[l].links[o]=link;
					}
				}
			link.clear();
			t_2hop.clear();
		}
		for(i=h.g.begin();i!=h.g.end();i++)
		{
			l=i->first;
			if(g[l].s)c.insert(l);
			i->second.s=g[l].s;
			i->second.ub=g[l].ub;
			deg_op(i->second.ub,u,0);
		}
		c.insert(v);
		h.g[v].s=1;
		for(i=h.g.begin();i!=h.g.end();i++)
			if(S<3)
			{
				i->second.get_deg(NULL);
				i->second.get_deg(&c);
			}
			else
			{
				deg_op(i->second.deg,u,0);
				deg_op(i->second.cdeg,u,0);
				if(i->second.links.find(v)!=i->second.links.end()||i->first==v)
				{
					if(i->second.adeg.empty())i->second.adeg.push_back(make_pair(h.ta,0));
					if(i->first!=v)deg_op(i->second.cdeg,l_to_d(i->second.links[v],-1),1);
				}
			}
		u.clear();
		if(c.size()<h.R.size())h.g.clear();
		c.clear();
		g.clear();
	}

	void del_v(int v)
	{
		for(map<int,vector<pair<int,int> > >::iterator i=g[v].links.begin();i!=g[v].links.end();i++)
		{
			int j=i->first;
			vector<pair<int,int> >deg=l_to_d(i->second,-1);
			deg_op(g[j].deg,deg,1);
			deg_op(g[j].cdeg,deg,1);
			g[j].links.erase(v);
		}
		g.erase(v);
	}
};

void read_graph(task &T)
{
	int t,i,k,s,c,ld,ta=inf,tb=0,m_d=int(r*(m_s-1)+1);
	map<int,vertex>::iterator v;
	map<int,vector<pair<int,int> > >::iterator j;
	FILE *f=fopen(f_name,"r");
	while(fscanf(f,"%d%d%d%d",&t,&i,&k,&c)>3)
	{
		if(i==k)continue;
		T.g[i].links[k].push_back(make_pair(t,c));
		T.g[k].links[i].push_back(make_pair(t,c));
		if(t<ta)ta=t;
		if(t>tb)tb=t;
	}
	fclose(f);
	T.ta=ta;
	T.tb=tb;
	for(v=T.g.begin();v!=T.g.end();v++)
	{
		for(j=v->second.links.begin();j!=v->second.links.end();j++)
		{
			if(j->second[j->second.size()-1].second)
			{
				j->second.push_back(make_pair(tb+1,0));
				T.tb=tb+1;
			}
			j->second=d_to_l(j->second,0);
		}
		v->second.get_deg(NULL);
	}
	map<int,vertex>g=T.g;
	for(v=g.begin();v!=g.end();v++)
	{
		v->second.deg=v->second.adeg;
		v->second.adeg.clear();
	}
	tb=T.tb;
	for(ld=m_d;;ld++)
	{
		printf("%02d,",ld%100);
		vector<pair<int,int> >remain_i;
		int u,n=g.size(),t,lt,d,re,re0;
		for(v=g.begin();v!=g.end();v++)
			if(v->second.adeg.empty())v->second.adeg.push_back(make_pair(ta,0));
		do
			for(u=0,v=g.begin();v!=g.end();v++)
				if(v->second.adeg.size())
				{
					printf("%02d%c%c",v->first%100,8,8);
					deg_op(v->second.deg,v->second.adeg,1);
					v->second.adeg.clear();
					s=v->second.deg.size();
					for(lt=ta,re0=1,d=k=0;k<s;k++)
					{
						t=v->second.deg[k].first;
						d=v->second.deg[k].second;
						re=1;
						if(d>0&&d<ld)re=0;
						if(re0&&!re)
						{
							if(t>lt)remain_i.push_back(make_pair(lt,t));
							lt=-1;
						}
						if(!re0&&re)lt=t;
						re0=re;
					}
					if(lt>=0&&tb>lt)remain_i.push_back(make_pair(lt,tb));
					if(remain_i.size()!=1||remain_i[0].first>ta||remain_i[0].second<tb)
					{
						u=1;
						i=v->first;
						deg_op(v->second.deg,l_to_d(remain_i,1),0);
						remain_i=d_to_l(v->second.deg,m_t);
						deg_op(v->second.deg,l_to_d(remain_i,1),0);
						for(j=v->second.links.begin();j!=v->second.links.end();j++)
						{
							k=j->first;
							int_op(j->second,remain_i,1);
							if(j->second!=g[k].links[i])
							{
								vector<pair<int,int> >redeg=l_to_d(g[k].links[i],-1);
								deg_op(redeg,l_to_d(j->second,1),1);
								g[k].links[i]=j->second;
								deg_op(g[k].adeg,redeg,1);
							}
						}
					}
					remain_i.clear();
				}
		while(u);
		clv(g,remain_i);
		if(ld==m_d)T.g=g;
		printf("%c%c%c",8,8,8);
		if(g.empty())break;
		for(v=g.begin();v!=g.end();v++)
		{
			vector<pair<int,int> >c=l_to_d(d_to_l(v->second.deg,m_t),1);
			deg_op(tc[v->first],c,1);
		}
	}
	for(j=tc.begin();j!=tc.end();j++)
	{
		for(k=0,s=j->second.size();k<s;k++)
		{
			int c=j->second[k].second;
			if(c)j->second[k].second=int((c+m_d-1)/r+1);
		}
		int v=j->first;
		T.g[v].ub=j->second;
		T.g[v].cdeg=T.g[v].deg;
	}
	T.U.push_back(make_pair(ta,int((ld-1)/r+1)));
	T.U.push_back(make_pair(tb,0));
}

void add_v(map<int,vertex>&ng,map<int,vertex>&g,int v)
{
	ng[v].ub=g[v].ub;
	for(map<int,vector<pair<int,int> > >::iterator i=g[v].links.begin();i!=g[v].links.end();i++)
	{
		int j=i->first;
		if(ng.find(j)==ng.end())continue;
		ng[v].links[j]=i->second;
		ng[j].links[v]=i->second;
		vector<pair<int,int> >deg=l_to_d(i->second,1);
		deg_op(ng[v].deg,deg,1);
		deg_op(ng[j].deg,deg,1);
		deg_op(ng[j].cdeg,deg,1);
		if(!g[j].s)deg_op(ng[v].cdeg,deg,1);
	}
}

pair<int,int>find_qc(task &T,int p,int h)
{
	pair<int,int>ret;
	vector<pair<int,int> >U;
	int i,n=T.g.size(),mh=-1,th=2,fh=1,ta,tb;
	if(n&&p)T.prune();
	n=T.g.size();
	if(!n||T.is_qc())return make_pair(0,1);
	map<int,vertex>g=T.g,ng;
	vector<int>V;
	i=T.bk_point();
	if(i<0)
	{
		int ms=T.R.size();
		if(ms<m_s)ms=m_s;
		for(;n>=ms;n--)
		{
			i=T.sel_v();
			if(i<0)
			{
				ng=T.g;
				U=T.U;
				ta=T.ta;
				tb=T.tb;
				T.prune();
				T.is_qc();
				break;
			}
			V.push_back(i);
			T.del_v(i);
		}
		if(n<ms)
		{
			ng=T.g;
			U=T.U;
			ta=T.ta;
			tb=T.tb;
		}
	}
	else
	{
		ng=T.g;
		U=T.U;
		ta=T.ta;
		tb=T.tb;
		task T2;
		T.div_t(T2,i);
		if(T2.tb-T2.ta>=m_t&&int(T2.g.size())>=m_s)
		{
			ret=find_qc(T2,0,h);
			fh&=ret.second;
			if(ret.first>mh)
			{
				mh=ret.first;
				th=1;
			}
		}
		T2.g.clear();
		if(T.tb-T.ta>=m_t&&int(T.g.size())>=m_s)
		{
			ret=find_qc(T,0,h);
			fh&=ret.second;
			if(ret.first>mh)
			{
				mh=ret.first;
				th=1;
			}
		}
	}
	T.ta=ta;
	T.tb=tb;
	T.U=U;
	for(i=V.size();i;)
	{
		if(!h&&!mh)return make_pair(0,0);
		if(time(NULL)-st_time>m_rt)return make_pair(mh+th/2,0);
		int v=V[--i];
		add_v(ng,g,v);
		time_t st=clock(),ed;
		T.g=ng;
		if(lv<877)printf("%02d",v%100);
		lv+=2;
		task T2;
		T.div_v(T2,v);
		ret=find_qc(T2,1,mh<h?h:h-1);
		fh&=ret.second;
		if(ret.first>=mh)
			if(ret.first>mh)
			{
				mh=ret.first;
				th=1;
			}
			else th=2;
		lv-=2;
		if(lv<877)printf("%c%c  %c%c",8,8,8,8);
		ed=clock();
	}
	V.clear();
	return make_pair(mh+th/2,fh);
}

void output()
{
	FILE *f=fopen(o_name,"a");
	fprintf(f,"Final coverage: %lld\n",minS+covS);
	for(int i=0,s=topK.size();i<s;i++)
	{
		fprintf(f,"size=%lu dur=%d ts=%d te=%d S={",topK[i].second.size(),topK[i].first.second-topK[i].first.first,topK[i].first.first,topK[i].first.second);
		for(int j=0,t=topK[i].second.size();j<t;j++)
		{
			if(j)fprintf(f,",");
			fprintf(f,"%d",topK[i].second[j]);
		}
		fprintf(f,"}\n");
	}
}

void del_qc(task &T,pair<pair<int,int>,vector<int> >qc)
{
	vector<pair<int,int> >u,d;
	if(qc.first.first>T.ta)u.push_back(make_pair(T.ta,qc.first.first));
	if(qc.first.second>T.tb)u.push_back(make_pair(qc.first.second,T.tb));
	for(int i=0,s=qc.second.size();i<s-1;i++)
	{
		int vi=qc.second[i];
		for(int j=i+1;j<s;j++)
		{
			int vj=qc.second[j];
			if(T.g[vi].links.find(vj)!=T.g[vi].links.end())
			{
				d=l_to_d(T.g[vi].links[vj],-1);
				int_op(T.g[vi].links[vj],u,1);
				deg_op(d,l_to_d(T.g[vi].links[vj],1),1);
				T.g[vj].links[vi]=T.g[vi].links[vj];
				deg_op(T.g[vi].adeg,d,1);
				deg_op(T.g[vi].acdeg,d,1);
				deg_op(T.g[vj].adeg,d,1);
				deg_op(T.g[vj].acdeg,d,1);
				d.clear();
			}
		}
	}
	clv(T.g,d);
}

task sT;

void get_topk(task T,int ms,int mt,int h)
{
	m_s=ms;
	m_t=mt;
	int sk=topK.size(),nk,i;
	find_qc(T,1,h);
	for(i=sk,nk=topK.size();i<nk;i++)
		del_qc(sT,topK[i]);
}

void sel_topk()
{
	int i,j,s=topK.size(),bj;
	if(s<K)return;
	long long bp,p;
	map<int,vector<pair<int,int> > >covS;
	vector<pair<pair<int,int>,vector<int> > >sel_topK;
	for(i=0;i<K;i++)
	{
		bp=0;
		for(j=0;j<s;j++)
		{
			p=priS(covS,topK[j]);
			if(p>bp)
			{
				bp=p;
				bj=j;
			}
		}
		if(!bp)break;
		sel_topK.push_back(topK[bj]);
		add_cov(covS,topK[bj]);
		for(j=bj;j<s-1;j++)
			topK[j]=topK[j+1];
		topK.resize(j);
	}
	topK=sel_topK;
	if(i<K)return;
	pair<pair<int,int>,vector<int> >rec=topK[K-1];
	topK.resize(K-1);
	updateK(rec);
}

int main(int argc, char** argv)
{
	st_time=time(NULL);
	sscanf(argv[1],"%s",f_name);
	sscanf(argv[2],"%lf",&r);
	r-=eps;
	sscanf(argv[3],"%d",&K);
	sscanf(argv[4],"%d",&m_s);
	sscanf(argv[5],"%d",&m_t);
	sscanf(argv[6],"%d",&m_rt);
	sprintf(o_name,"%.2lf,%d,%d,%d,%d,%s",r,K,m_s,m_t,m_rt,f_name);
//	sprintf(o_name,"qc.txt");
	task T,T2;
	read_graph(T);
	FILE *f=fopen(o_name,"w");
	fprintf(f,"%ld sec: finished loading graph and computing temporal core\n",long(time(NULL)-st_time));
	fclose(f);
	int ms=m_s;
	int mt=m_t;
	int oK=K;
	sT=T;
	K=inf;
	if(oK<inf)get_topk(sT,ms,mt+mt/2,2);
	if(oK<inf)get_topk(sT,ms,mt+mt/5,2);
	get_topk(sT,ms,mt,1);
	K=oK;
	sel_topk();
	m_s=ms;
	m_t=mt;
	sT.g.clear();
	printf("%lld %lld\n",covS,minS);
	for(int h=2;;h++)
	{
		stop=1;
		T2=T;
		printf("%02d|",h);
		lv+=3;
		pair<int,int>ret=find_qc(T2,1,h);
		f=fopen(o_name,"a");
		time_t rt=time(NULL)-st_time;
		fprintf(f,"%ld sec: finished searching level %d",long(rt),h);
		if(rt>m_rt)fprintf(f," (time out)\n");
		else fprintf(f,"\n");
		fclose(f);
		lv-=3;
		if(rt>m_rt||ret.second||stop&&m_rt==inf)break;
	}
	output();
	return 0;
}
