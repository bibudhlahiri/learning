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
  bool operator()(string s1, string s2) const
  {
    return (s1 == s2);
  }
};


/*The following namespace ensures that the hash_map class works even with string, not 
only const char**/
namespace __gnu_cxx
{
	template<> struct hash< std::string >
	{
		size_t operator()( const std::string& x ) const
		{
		  return hash< const char* >()( x.c_str() );
		}
	};
}


static const char alphanum[] =
  "0123456789"
  "!@#$%^&*"
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "abcdefghijklmnopqrstuvwxyz";

int stringLength = sizeof(alphanum) - 1;

typedef hash_map<string, int, hash<string>, eqstr> HashMapForTest;
typedef HashMapForTest::key_type KeyType;
typedef list<KeyType> ListOfKeys;
typedef HashMapForTest::iterator IterForKeys;

class SpeedTest
{
  HashMapForTest hashMapForTest;
  
  public:
    SpeedTest();
    void getKeySet(ListOfKeys&);
    char genRandom() 
    {
      return alphanum[rand() % stringLength];
    }
    void decrementFrequency(const KeyType&);
    void makeSinglePass();
    void testUpdateSpeed();

  };

 void SpeedTest::getKeySet(ListOfKeys& keySet)
 {
    for (IterForKeys iterForKeys = hashMapForTest.begin(); 
          iterForKeys != hashMapForTest.end(); ++iterForKeys)
    {
      KeyType key = iterForKeys->first; 
      keySet.push_back(key);
    } 
 }

 SpeedTest::SpeedTest()
 {
    //Populate the map with random <key, value> pairs
    srand(time(0));
    for (int j = 0; j < 10000; j++)
    {
      //Generate the key as a random string
      std::string key;
      for(unsigned int i = 0; i < 20; ++i)
      {
        key += genRandom();
      }
      //Generate the frequency as a random integer between 1000 and 4999
      int frequency = rand() % 4000 + 1000;
      hashMapForTest[key] = frequency;
    }
 }

 void SpeedTest::decrementFrequency(const KeyType& key)
 {
    int frequency = hashMapForTest[key];
    frequency--;
    hashMapForTest[key] = frequency;
 }


 void SpeedTest::makeSinglePass()
  {
    ListOfKeys listOfKeys;
    getKeySet(listOfKeys);
    int count = 0;
    while (!listOfKeys.empty())
    {
       KeyType key = listOfKeys.front();
       decrementFrequency(key);
       listOfKeys.pop_front();
       ++count;
    }
    cout << "Processed " << count << " records" << endl;
  }

  void SpeedTest::testUpdateSpeed()
  {
    int nIter = 100;
    double sumTime = 0;
    for (int j = 0; j < nIter; j++)
    {
      clock_t startTime = clock();
      makeSinglePass();
      double timeTaken = double(clock() - startTime)/(double)CLOCKS_PER_SEC;
      cout << timeTaken << " seconds" << endl;
      sumTime += timeTaken;
    }
    cout << "Avg time to update tuples = " << (double)sumTime/(double)nIter << " seconds" << endl;
  }


int main()
{
  SpeedTest st;
  st.testUpdateSpeed();
  return 0;
}




   

