#include <iostream>
#include <string>
#include <cstdlib>
#include <ctime>
#include <time.h>
#include <google/sparse_hash_map>
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
	template<> struct std::tr1::hash< std::string >
	{
		size_t operator()( const std::string& x ) const
		{
		  return std::tr1::hash< const char* >()( x.c_str() );
		}
	};
}


static const char alphanum[] =
  "0123456789"
  "!@#$%^&*"
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "abcdefghijklmnopqrstuvwxyz";

int stringLength = sizeof(alphanum) - 1;

typedef google::sparse_hash_map<string, int, std::tr1::hash<string>, eqstr> HashMapForTest;
typedef HashMapForTest::key_type KeyType;
typedef list<KeyType> ListOfKeys;
typedef HashMapForTest::iterator IterForKeys;

class FastSpeedTest
{
  HashMapForTest hashMapForTest;
  
  public:
    FastSpeedTest();
    void getKeySet(ListOfKeys&);
    char genRandom() 
    {
      return alphanum[rand() % stringLength];
    }
    void decrementFrequency(const KeyType&);
    int getCountOfKey(const KeyType&);
    void makeSinglePass();
    void testUpdateSpeed();
};

 void FastSpeedTest::getKeySet(ListOfKeys& keySet)
 {
    for (IterForKeys iterForKeys = hashMapForTest.begin(); 
          iterForKeys != hashMapForTest.end(); ++iterForKeys)
    {
      KeyType key = iterForKeys->first; 
      keySet.push_back(key);
    } 
 }

 FastSpeedTest::FastSpeedTest()
 {
    //Populate the map with random <key, value> pairs
    srand(time(0));
    for (int j = 0; j < 3000; j++)
    {
      //Generate the key as a random string
      std::string key;
      for(unsigned int i = 0; i < 20; ++i)
      {
        key += genRandom();
      }
      //Generate the frequency as a random integer between 10 and 4999
      int frequency = rand() % 4000 + 10;
      hashMapForTest[key] = frequency;
      /*if (frequency < 100)
      {
        cout << "key with low freq is " << key << endl;
      }*/
    }
 }

 void FastSpeedTest::decrementFrequency(const KeyType& key)
 {
    int frequency = hashMapForTest[key];
    frequency--;
    hashMapForTest[key] = frequency;
 }

 int FastSpeedTest::getCountOfKey(const KeyType& key)
 {
    return hashMapForTest[key];
 }


 void FastSpeedTest::makeSinglePass()
  {
    ListOfKeys listOfKeys;
    getKeySet(listOfKeys);
    int count = 0;
    ListOfKeys keysToDelete;

    while (!listOfKeys.empty())
    {
       KeyType key = listOfKeys.front();
       decrementFrequency(key);
       /*Average reduced from 0.00850618 seconds to 0.00840847 seconds when call to decrementFrequency 
         was replaced by three lines directly
       int frequency = hashMapForTest[key];
       frequency--;
       hashMapForTest[key] = frequency;*/
       
       if (getCountOfKey(key) == 0)
       {
         keysToDelete.push_back(key);
       }
       listOfKeys.pop_front();
       ++count;
    }
    //cout << "Processed " << count << " records" << endl;
    while (!keysToDelete.empty())
    {
      KeyType key = keysToDelete.front();
      hashMapForTest.erase(key);
      //cout << "Erasing key " << key << endl;
      keysToDelete.pop_front();	
    }
  }

  void FastSpeedTest::testUpdateSpeed()
  {
    int nIter = 100;
    double sumTime = 0;
    for (int j = 0; j < nIter; j++)
    {
      clock_t startTime = clock();
      makeSinglePass();
      double timeTaken = double(clock() - startTime)/(double)CLOCKS_PER_SEC;
      //cout << timeTaken << " seconds" << endl;
      sumTime += timeTaken;
    }
    /*Without dropping the keys whose frequencies drop to 0, avg time with 10,000 pairs: 0.00850618 seconds. 
      Avg time with 5,000 pairs: 0.00409858 seconds.
      Avg time with 3,000 pairs: 0.00245966 seconds.

    /With dropping the keys whose frequencies drop to 0, avg time with 3,000 pairs: 0.00288443 seconds.*/
    cout << "Avg time to update tuples = " << (double)sumTime/(double)nIter << " seconds" << endl;
  }


int main()
{
  FastSpeedTest st;
  st.testUpdateSpeed();
  return 0;
}




   

