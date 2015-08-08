#include <iostream>
#include <vector>
#include "json.hpp"
#include <unistd.h>
#include <cstdio>
#include <cassert>
#include <fstream>

using json = nlohmann::json;;
using namespace std;

#define FOR(i, n) for(long long int i = 0; i < (n); ++i)

using pos = pair<int, int>;


struct unit{
  vector<pos> members;
  pos pivot;
};

struct problem {
  int id;
  vector<unit> units;
  int width, height;
  int sourceLength;
  vector<int> sourceSeeds;
};

template<typename T>
ostream& operator <<(ostream& s, vector<T> const& v){
  long long int sz = v.size();
  FOR(i, sz) {
    s << v[i] << ' ';
  }
  return s;
}

vector<vector<bool>> initialMap(problem const& p){
  vector<vector<bool>> v(p.height);
  
};

void printMap(vector<vector<bool>> const& map)

ostream& operator <<(ostream& s, problem const& pb){
  s << "problem ID=" << pb.id << " SIZE=" << pb.width << "x" << pb.height << " NSEEDS=" << pb.sourceSeeds.size() << endl;

  return s;
}

//

void process_file(string const &filename){
  fstream is(filename);
  json d; is >> d;
  problem pb;
  pb.id = d["id"];
  for(auto const& v : d["units"]){
    unit u;
    for(auto const& a : v["members"]){
      u.members.push_back(make_pair(a["pivot"]["x"], a["pivot"]["y"]));
    }
    u.pivot = make_pair(v["pivot"]["x"], v["pivot"]["y"]);
    pb.units.push_back(u);
  }
  pb.width = d["width"];
  pb.height = d["height"];
  pb.sourceLength = d["sourceLength"];
  for(auto const& v : d["sourceSeeds"]){
    pb.sourceSeeds.push_back(v);
  }

  cout << pb << endl;
}

int main(int argc, char** argv){
  vector<string> files;
  { int c;
    while ((c = getopt (argc, argv, "f:")) != -1){
      switch (c){
      case 'f':
        files.push_back(optarg);
        break;
      }
    }
    for(int i = optind; i < argc; i++)
      files.push_back(argv[i]);
  }
  for(auto const& fn : files){
    process_file(fn);
  }
  return 0;
}
