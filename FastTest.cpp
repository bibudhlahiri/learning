#include <iostream>
#include <google/sparse_hash_map>
using namespace std;

    struct eqstr
    {
      bool operator()(const char* s1, const char* s2) const
      {
        return (s1 == s2) || (s1 && s2 && strcmp(s1, s2) == 0);
      }
    };

    int main()
    {
      google::sparse_hash_map<const char*, int, std::tr1::hash<const char*>, eqstr> months;

      months.set_deleted_key(NULL);
      months["january"] = 31;
      months["february"] = 28;
      months["march"] = 31;
      months["april"] = 30;
      months["may"] = 31;
      months["june"] = 30;
      months["july"] = 31;
      months["august"] = 31;
      months["september"] = 30;
      months["october"] = 31;
      months["november"] = 30;
      months["december"] = 31;

      std::cout << "september -> " << months["september"] << std::endl;
      std::cout << "april     -> " << months["april"] << std::endl;
      std::cout << "june      -> " << months["june"] << std::endl;
      std::cout << "november  -> " << months["november"] << std::endl;
    }
