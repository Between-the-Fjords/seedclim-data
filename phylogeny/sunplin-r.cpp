#include <set>
#include <list>
#include <stack>
#include <cmath>
#include <queue>
#include <ctime>
#include <cfloat>
#include <vector>
#include <string>
#include <cstdio>
#include <bitset>
#include <climits>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <iomanip>
#include <sstream>
#include <utility>
#include <fstream>
#include <iostream>
#include <algorithm>
#include <ctype.h>
#include <R.h>
#include <Rdefines.h>
#include <sys/timeb.h>
using namespace std;

struct timeb ini, fim;
const double INF = 1e50;
const int MAXN = 500010;
const int MAXGENERATENAMES = 1000;
const int MAXV = 98;
const  double EPS = 1e-9;
int cmp(double a, double b = 0.0){ if(fabs(a-b) < EPS) return 0; return a > b ? 1 : -1; }

class no{
	public:
		int x;
		double b;
		no (int x = 0, double b = 0) : x(x) , b(b) {}
};


class insert{
	public:
		string name;
		int insertionPoint;
		insert(string name = "", int insertionPoint = 0) : name(name), insertionPoint(insertionPoint) { }
};

class subTree{
	public:
		double sumSubTree;
		int sizeSubTree, rootCount;
		subTree(double sumSubTree = 0, int sizeSubTree = 0, int rootCount = 0) : sumSubTree(sumSubTree), sizeSubTree(sizeSubTree), rootCount(rootCount) { }
};

class infoNode{
	public:
		double valor;
		int id;
		infoNode( double valor = 0., int id = 0 ) : valor(valor), id(id) { }
};

bool compara( const infoNode &b, const infoNode &a ) {
	if( cmp( b.valor, a.valor ) > 0 ) return false;
	if( cmp( b.valor, a.valor ) == 0 ) return b.id < a.id;
	return true;
}
int nodedad[ MAXN ];
int treesize[ MAXN ];
int chain[ MAXN ];
int homepos[ MAXN ];
int pos, cntchain;
int chainleader[ MAXN ];
char taxon[500000][100], line[MAXN*10], taxonCopia[500000][100], version;
double bl[MAXN], blCopy[MAXN];
int  up[MAXN], down[7000][7000], sumAux[MAXN], noat[MAXN], depth[MAXN], nodes, sometimesGenerates, treeAmount, amountNewNames = 0, camNode = 0, arvoresQt = 0;
bool dfstop;
int parsingTree();
string newSpeciesFile(char local[100]);
void insertSpecies(int u, string name);
string convertNewickToNexus(int op, string nw);
void printTime();
int dfs_insert_V1( int u );
void BFS();
double dfs_sum_V2( int u );
void generateNamesSpecies();
void errorType(string s1, string s2, string s3);
string grafoToNewick(int u);
subTree sum[MAXN];
vector < insert > espN; 
vector < infoNode > nodeInfo;
vector < insert > espNames; 
vector < vector<int > > grafo, grafoMatrix;
vector < string > nameRand;
vector < int >  father;
void explore( int x, int dad );
void heavy_light( int x, int dad, int k, int p );
int lca(int a, int b) ;
char filename[MAXN*10];

extern "C" SEXP dist(SEXP file, SEXP option){
	int leaves = 0;
	char op[100];
	SEXP mat;
	PROTECT(file = AS_CHARACTER(file));
	PROTECT(option = AS_CHARACTER(option));
	strcpy(filename, CHAR(STRING_ELT(file,0)));
	strcpy(op, CHAR(STRING_ELT(option,0)));
	if( !strcmp(op,"file") ){
		fstream arquivo (filename, fstream::in | fstream::out);
		if(!arquivo.good()){ cout << "Error! The file " << filename << " does not exist\n"; PROTECT(mat = allocMatrix(REALSXP, 0, 0));	UNPROTECT(3); return mat; }
		arquivo >> line;
	}
	else if( !strcmp(op,"tree") ){
		strcpy(line, filename);
	}
	else{
		cout << "Error! Invalid option. Please choose 'tree' or 'file' as an option\n";
		PROTECT(mat = allocMatrix(REALSXP, 0, 0));	
		UNPROTECT(3);
		return mat;
	}
	parsingTree();
	for( int i = 0; i < nodes; i++ ) if( !noat[i] ) leaves++;
	PROTECT(mat = allocMatrix(REALSXP, leaves, leaves));	
	for(int ss = 0 ; ss < 1; ss++){
		int  k, w;
		int  d;
		for(int we = 0; we <= nodes; we++) nodedad[we] = -1;
		explore( 1, 0 );
		pos = 0, cntchain = 0;
		heavy_light( 1, 0, -1, 0 );
		int aa = 0, bb = 0;
		for (k = 1; k <= nodes; k++){
			REAL(mat)[aa + leaves*aa] = 0.;
			if(!noat[k-1]){
				bb = aa + 1;
				for (w = k+1; w <= nodes; w++) {
					if(!noat[w-1]){
						 d = lca(k,w);
						  REAL(mat)[(aa) + leaves * (bb)] = REAL(mat)[(bb) + leaves * (aa)] = bl[k-1] + bl[w-1] - 2.0*bl[d-1];
						bb++;
					}
				}
				aa++;
			}
			bb = 0;
		}
	}
	UNPROTECT(3);	
	return mat;
}

extern "C" SEXP expd(SEXP file, SEXP newSpecie, SEXP vz, SEXP v){
	generateNamesSpecies();
	srand ( time(NULL) );
	char filename[400], novasEspecies[400];
	int *pNum, *ver;
	string ret = "";
	SEXP ans = PROTECT(NEW_CHARACTER(1000000));
	PROTECT(v = AS_INTEGER(v));
	PROTECT(file = AS_CHARACTER(file));
	PROTECT(newSpecie = AS_CHARACTER(newSpecie));
	PROTECT(vz = AS_INTEGER(vz));
	strcpy(filename, CHAR(STRING_ELT(file,0)));
	strcpy(novasEspecies, CHAR(STRING_ELT(newSpecie,0)));
	pNum = INTEGER(vz);
	ver = INTEGER(v);
	version = '0'+(*ver);
	sometimesGenerates = *pNum;
	fstream arquivo (filename, fstream::in | fstream::out);
	if(!arquivo.good()){ cout << "Error! The file " << filename << " does not exist\n"; UNPROTECT(5); return ans; }
	arquivo >> line;
	arquivo.close();
	if( parsingTree() ){ UNPROTECT(5); return ans; }
	ret = newSpeciesFile(novasEspecies);
	if( ret != "-1" )
		ans = mkString(ret.c_str());
	UNPROTECT(5);
	return ans;
}


void limpar(){ for(int i = 0; i < nodes+1; i++) bl[i] = 0.; grafo.clear(); nodes = 0;}

void generateNamesSpecies(){
	char alphabet[ ] = {'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'};
	string nameEspecie = "";
	char numberAux[100];
	for(int i = 0; i < MAXGENERATENAMES; i++){
		sprintf(numberAux, "%d", i);
		for( int j = 0; j < 26; j++ ){
			nameEspecie += alphabet[j];
			nameRand.push_back(nameEspecie + numberAux);
			nameEspecie = "";
		}
	}
}

void printTime(){ cout << "Time : "<< ((double) fim.time + ((double) fim.millitm * 0.001)) - ((double) ini.time + ((double) ini.millitm * 0.001)) << "\n"; }

string convertNewickToNexus(int op, string nw){
	string ret = "";
	char numBer[400];
	switch (op){
		case 0: ret += "begin trees;\n"; break;
		case 1: 
			sprintf(numBer, "%d", arvoresQt); 
			arvoresQt++; 
			ret += "TREE tree";
			ret += numBer;
			ret += " = ";
			ret += nw;
			ret += ";\n";
			break;
		case 2: 
			ret += "end;"; 
			break;
	}
	return ret;
}

void errorType(string s1, string s2, string s3){
	if(s1 != "") printf("%s",s1.c_str());
	if(s2 != "") printf("%s",s2.c_str());
	if(s3 != "") printf("%s",s3.c_str());
}

int parsingTree(){
	int n = 0, i = 0, nodei = -1, leni = -1, atn = 0, branchlengths = 0, done = 0;
	int lbrack, rbrack, comma, quant;
	char ch, taxa[1000];
	comma = quant = nodes = rbrack = lbrack = 0;
	quant = strlen(line);
	for(int i = 0; i < quant; i++){
		ch = line[i];
		if(ch == '(') lbrack++;
		if(ch == ')') rbrack++;
		if(ch == ',') comma++;
	}
	if(lbrack != rbrack){ errorType("Unbalanced parenthesis.\n","",""); return 1; }
	nodes = lbrack + comma + 1;
	line[quant-1] = 59;
	line[quant] = 0;
	grafo.clear();
	grafoMatrix.clear();
	grafo.resize(nodes+200);
	grafoMatrix.resize(nodes+200);
	father.resize(nodes+200);
	while(line[n] != ';') n++;
	while (i < n) {
		done = 0;
		if (line[i] == '('){ // "(" 
			nodei++;
			if(leni!=-1){
				noat[leni]++;
				grafo[leni+1].push_back(nodei+1);
				grafoMatrix[leni+1].push_back(nodei+1);
				father[nodei+1] = leni+1;
			}
			up[nodei] = leni;
			strcat(taxon[nodei], ".");
			leni = nodei;
			done = 1;
			i++;
		}
		else if (line[i] == ','){ // ","
			done = 1;
			i++;
		}
		else if (line[i] == ')'){ // ")"
			leni = up[leni];
			atn = up[atn];
			done = 1;
			i++;
		}
		else if ((((line[i] >= 65) && (line[i] <= 90)) || ( (line[i] >= 97) && (line[i] <= 122)) || (line[i] == 45) || (line[i] == 95)) && (line[i-1] == ')')){
			string name = "";
			while ((line[i] != ':') && (line[i] != ',') && (line[i] != ')') && (line[i] != '[') && (line[i] != ';') && (line[i] != ']')) {
				name += line[i];
				i++;
			}
			strcpy(taxon[atn], name.c_str());
			done = 1;
		}
		else if (line[i] == '['){
			while (line[i] == ']') i++;	
			i++;
			done = 1;
		}
		else if (line[i] == ':'){
			string num = "";
			i++;
			while(((line[i] >= 48) && (line[i] <= 57)) || (line[i] == 46) || (line[i] == 'E') || (line[i] == 'e') || (line[i] == '-')){
				num += line[i];
				i++;
			}
			if (num[0] != 0) branchlengths = 1;
			sscanf((char*)num.c_str(), "%lf", &bl[atn]);
			done = 1;
		}
		while ((line[i] == ' ') || (line[i] == '\t') || (line[i] == '\n') || (line[i] == '\r')) i++;
		if (done == 0){
			string tax = "";
			tax += line[i++];
			while ((line[i] != ',') && (line[i] != ')') && (line[i] != ':') && (line[i] != '[')){
				tax += line[i++];
			}
			strcpy(taxa,tax.c_str());
			nodei++;
			atn = nodei;
			up[nodei] = leni;
			strcpy(taxon[nodei], taxa );
			noat[leni]++;
			grafo[leni+1].push_back(nodei+1);
			grafoMatrix[leni+1].push_back(nodei+1);
			father[nodei+1] = leni+1;
		}
	}
	for (i = 0; i <= nodei; i++)
		if (branchlengths == 0) bl[i] = 1.0;
	
	for (i = 0; i < nodes; i++){ 
		if (strcmp(taxon[i], ".") == 0) strcpy(taxon[i], "");
	}
	return 0;
}


string grafoToNewick(int u){
	int tam = grafo[u].size();
	if(!tam) return "";
	string str = "(";
	char num[100];
	for(int i = 0; i < tam; i++){
		string ret = grafoToNewick(grafo[u][i]); 
		str += ret;
		str += taxon[grafo[u][i]-1];
		sprintf(num, "%0.8lf", bl[grafo[u][i]-1]);
		str += ":";
		str += num;
		if(i + 1 != tam) str += ",";
	}
	str += ")";
	return str;
}

int contId = 0;

int searchNameEspecie(char *str){
	for(int i = 0; i < nodes; i++) 
		if( !strcmp(str, taxon[i]) ) return (i + 1);
	errorType("Error! The specie ",str," not belong to tree!");
	return -1;
}

string newSpeciesFile(char local[100]){
	ifstream in( local , ifstream::in );
	if(!in.good()){ errorType("Error opening file in place: ",local,"\n"); return "-1"; }
	//fstream filestr ("out.nex", fstream::out);
	string out = "", aux = "";
	out = convertNewickToNexus(0,"");
	vector < vector<int > > grafoBackup ( grafo.begin(), grafo.end());
	vector < int > fatherBackup( father.begin(), father.end() );
	memcpy(blCopy,bl,sizeof(blCopy));
	vector < infoNode > :: iterator it;
	int copyNodes = nodes;
	nodeInfo.resize(nodes + 10);
	char name[1000], insertionPoint[1000];
	if( version == '2' ) dfs_sum_V2(1);
	else dfs_insert_V1(1);
	while(in >> name >> insertionPoint){
		int ptInsercao = searchNameEspecie(insertionPoint);
		if( ptInsercao < 0 ) return "-1";
		espN.push_back(insert(name,ptInsercao));
		espNames.push_back(insert(name,ptInsercao));
	}
	for(int i = 0;  i < sometimesGenerates; i++){
		amountNewNames = 0;
		for(int j = 0; j < nodes+10; j++) bl[j] = blCopy[j]; 
		if( version == '2' ) {
			for(int j = 0; j < (int)espN.size(); j++){
				double calc = nodeInfo[(sum[espN[j].insertionPoint-1].sizeSubTree-1) + espN[j].insertionPoint].valor - nodeInfo[espN[j].insertionPoint].valor;
				calc *= (((double)(rand() % MAXV) + 1.0)/100.0);
				calc += nodeInfo[espN[j].insertionPoint].valor;
				it = upper_bound(nodeInfo.begin() + espN[j].insertionPoint, nodeInfo.begin() + espN[j].insertionPoint + (sum[espN[j].insertionPoint-1].sizeSubTree-1), calc, compara);
				insertSpecies(it->id, espN[j].name);
			}
		}
		else
			for( int j = 0; j < (int)espNames.size(); j++) 
				insertSpecies((rand()%sumAux[espNames[j].insertionPoint]) + espNames[j].insertionPoint, espNames[j].name);

		aux = convertNewickToNexus(1, grafoToNewick(1));
		out += aux;
		grafo.clear();
		father.clear();
		grafo.assign(grafoBackup.begin(), grafoBackup.end());
		father.assign(fatherBackup.begin(), fatherBackup.end());
		nodes = copyNodes;
	}
	aux = convertNewickToNexus(2, "");
	out += aux;
	return out;
}


void removeAresta(int fatherAux, int child){
	for(int i = 0; i < (int)grafo[fatherAux].size(); i++){
		if(grafo[fatherAux][i] == child){
			grafo[fatherAux].erase(grafo[fatherAux].begin() + i);
			break;
		}
	}
}

double custoSubCadeia(int u){
	if( !grafo[u].size() ) return 0.; 
	int pos = rand() % grafo[u].size();
	return (custoSubCadeia(grafo[u][pos]) + bl[grafo[u][pos]-1]);
}


void insertSpecies(int u, string name){
	int fatherAux = father[u];
	double R, value;
	if(! grafoMatrix[u].size()){
		removeAresta(fatherAux, u);
		grafo[fatherAux].push_back(nodes + 1);
		father[nodes+1] = fatherAux;
		grafo[nodes + 1 ].push_back(u);
		father[u] = nodes + 1;
		grafo[nodes + 1 ].push_back(nodes + 2 );
		father[nodes+2] = nodes+1;
		R = ((double)(rand() % MAXV) + 1.0)/100.0;
		value = bl[u-1];
		value *= R;
		bl[nodes] = value;
		bl[nodes + 1] = bl[u - 1] = fabs(value - bl[u - 1]);
		string novaEspecie = nameRand[amountNewNames++];
		strcpy(taxon[nodes],novaEspecie.c_str());
		strcpy(taxon[nodes+1],name.c_str());
	}
	else if( grafoMatrix[u].size() == 1){
		grafo[u].push_back(nodes + 1);
		father[nodes+1] = u;
		bl[nodes] = bl[grafo[u][0] - 1];
		strcpy(taxon[nodes],name.c_str());
	}
	else{
		int child = rand() % grafo[u].size();
		double division;
		value = sum[u - 1].sumSubTree / double(sum[u - 1].rootCount); 
		R = ((double)(rand() % MAXV) + 1.0)/100.0;
		division = bl[grafo[u][child] - 1];
		division *= R;
		bl[nodes] = division;
		bl[nodes + 1] = fabs(value - division);
		if( bl[nodes + 1]  < EPS ) bl[nodes + 1] = value;
		bl[grafo[u][child] - 1] = fabs(bl[grafo[u][child] - 1] - division); 
		string novaEspecie = nameRand[amountNewNames];
		strcpy(taxon[nodes],novaEspecie.c_str());
		strcpy(taxon[nodes+1],name.c_str());
		int fi = grafo[u][child];
		grafo[u].erase(grafo[u].begin() + child);
		grafo[u].push_back(nodes + 1 );
		father[nodes + 1] = u;
		grafo[nodes + 1 ].push_back(nodes + 2 );
		father[nodes + 2] = nodes + 1;
		grafo[nodes + 1 ].push_back(fi);
		father[fi] = nodes + 1;
	}
	nodes += 2;
	if(nodes+2 >= grafo.size()) grafo.resize(grafo.size() + 100); 
	if(nodes+2 >= father.size()) father.resize(father.size() + 100); 
}


int dfs_insert_V1( int u ){
	int sAux = 1;
	sum[u-1].rootCount = (grafoMatrix[u].size() == 0);
	sum[u-1].sumSubTree = 0.;
	for(int i = 0; i < (int)grafoMatrix[u].size(); i++){
		int at = grafoMatrix[u][i];
		sAux += dfs_insert_V1(at);
		sum[u-1].rootCount += sum[at-1].rootCount;
		sum[u-1].sumSubTree += ((double)sum[at-1].rootCount * bl[at-1]) + sum[at-1].sumSubTree;
	}
	sumAux[u] = sAux;
	return sAux;
}

double custAnt = 0.;

double dfs_sum_V2( int u){
	double sumAux = bl[u-1];
	camNode++;
	custAnt += bl[u-1];
	if( u != 1 ){ nodeInfo[u].valor = custAnt; nodeInfo[u].id = u ; }
	sum[u-1].sizeSubTree = 1;
	sum[u-1].rootCount = (grafoMatrix[u].size() == 0);
	sum[u-1].sumSubTree = 0.;
	for( int i = 0; i < (int)grafoMatrix[u].size(); i++){
		int at = grafoMatrix[u][i];
		sumAux += bl[at-1];
		dfs_sum_V2(at);
		sum[u-1].rootCount += sum[at-1].rootCount;
		sum[u-1].sizeSubTree += sum[at-1].sizeSubTree;
		sum[u-1].sumSubTree += ((double)sum[at-1].rootCount * bl[at-1]) + sum[at-1].sumSubTree;
	}
	return (sumAux);
}


void explore( int x, int dad ) {
    if( nodedad[x] != -1 ) return;
    nodedad[x] = dad;
    treesize[x] = 1;
    for( int i = 0; i < ( int )grafo[x].size(); ++i ){
        if( grafo[x][i] != dad ) {
			bl[grafo[x][i] - 1] += bl[ x - 1];
            explore( grafo[x][i], x );
            treesize[x] += treesize[ grafo[x][i] ];
        }
	}
}

void heavy_light( int x, int dad, int k, int p ){
    if( !p ){ k = cntchain++; chainleader[k] = x; }
    chain[x] = k;
    homepos[x] = pos++;
    int mx = -1;
    for( int i = 0; i < ( int )grafo[x].size(); ++i )
        if(grafo[x][i] != dad && (mx == -1 || treesize[grafo[x][i]] > treesize[ grafo[x][mx]])) mx = i;
    if( mx != -1 ) heavy_light( grafo[x][mx], x, k, p+1 );
    for( int i = 0; i < ( int )grafo[x].size(); ++i )
        if( grafo[x][i] != dad && i != mx ) heavy_light( grafo[x][i], x, -1, 0 );
}

int lca(int a, int b) {
	while (chain[a] != chain[b]) {
		if (treesize[chainleader[chain[a]]] >= treesize[chainleader[chain[b]]]) b = nodedad[chainleader[chain[b]]];
		else a = nodedad[chainleader[chain[a]]];
	}
	if (treesize[a] < treesize[b]) return b;
	return a;
}

