#include <iostream>
#include <string>
#include <cstdlib>
#include <ctime>
#include <time.h>
#include <backward/hash_map.h>
#include <backward/list.h>
using namespace std;

/*Will insert a few thousand random <key, count> pairs to a hashtable. Will iterate a few times over over these pairs, and in each iteration,
 will decrement the count values and push them back to the hashtable. Will measure the time for each iteration and will average those time values.*/

struct eqstr
{
  bool operator()(const char* s1, const char* s2) const
  {
    return strcmp(s1, s2) == 0;
  }
};

static const char alphanum[] =
  "0123456789"
  "!@#$%^&*"
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "abcdefghijklmnopqrstuvwxyz";

int stringLength = sizeof(alphanum) - 1;

typedef hash_map<const char*, int, hash<const char*>, eqstr> HashMapForTest;
typedef list<const char*> ListOfKeys;
typedef HashMapForTest::iterator IterForKeys;

class SpeedTest
{
  HashMapForTest hashMapForTest;

  //int stringLength;
  
  public:
    SpeedTest();
    void getKeySet(ListOfKeys&);
    char genRandom() 
    {
      return alphanum[rand() % stringLength];
    }
    void decrementFrequency(const char*);
    void makeSinglePass();
    void testUpdateSpeed();

  };

 void SpeedTest::getKeySet(ListOfKeys& keySet)
 {
    for (IterForKeys iterForKeys = hashMapForTest.begin(); 
          iterForKeys != hashMapForTest.end(); ++iterForKeys)
    {
      const char* key = iterForKeys->first; 
      keySet.push_back(key);
    } 
 }

 SpeedTest::SpeedTest()
 {
    
    //Populate the map with random <key, value> pairs
    srand(time(0));
    for (int j = 0; j < 100; j++)
    {
      //Generate the key as a random string
      std::string key;
      for(unsigned int i = 0; i < 20; ++i)
      {
        key += genRandom();
      }
      //Generate the frequency as a random integer between 1000 and 4999
      int frequency = rand() % 4000 + 1000;
      cout << "Inserting key = " << key << ", frequency = " << frequency << endl;
      hashMapForTest[key.c_str()] = frequency;
    }
 }

 void SpeedTest::decrementFrequency(const char* key)
 {
    int frequency = hashMapForTest[key];
    cout << "key = " << key << ", frequency = " << frequency << endl;
    frequency--;
    hashMapForTest[key] = frequency;
 }


 void SpeedTest::makeSinglePass()
  {
    ListOfKeys listOfKeys;
    getKeySet(listOfKeys);
    while (!listOfKeys.empty())
    {
       //Decrement the frequency of the key, pointed to by iterSecondAttr, by 1
       const char* key = listOfKeys.front();
       decrementFrequency(key);
       listOfKeys.pop_front();
    }
  }

  void SpeedTest::testUpdateSpeed()
  {
    int nIter = 100;
    for (int j = 0; j < nIter; j++)
    {
      clock_t startTime = clock();
      makeSinglePass();
      cout << double( clock() - startTime ) / (double)CLOCKS_PER_SEC<< " seconds." << endl;
    }
  }


int main()
{
  SpeedTest st;
  st.testUpdateSpeed();
  return 0;
}




   

