#include <iostream>
#include <string>
#include <cstdlib>
#include <ctime>
#include <time.h>
#include <google/sparse_hash_map>
#include <backward/list.h>
#include <functional>
using namespace std;

/*Will insert a few thousand random <key, count> pairs to a hashtable. Will iterate a few times over over these pairs, and in each iteration,
 will decrement the count values and push them back to the hashtable. Will measure the time for each iteration and will average those time values.*/

struct eqstr
    {
      bool operator()(const char* s1, const char* s2) const
      {
        return (s1 == s2) || (s1 && s2 && strcmp(s1, s2) == 0);
      }    
 };

static const char alphanum[] =
  "0123456789"
  "!@#$%^&*"
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "abcdefghijklmnopqrstuvwxyz";

int stringLength = sizeof(alphanum) - 1;

typedef google::sparse_hash_map<const char*, int, std::tr1::hash<const char*>, eqstr> HashMapForTest;
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
    char key[] = "";
    hashMapForTest.set_deleted_key(NULL);
    for (int j = 0; j < 10000; j++)
    {
      //Generate the key as a random string
      unsigned int i;
      for (i = 0; i < 20; ++i)
      {
        key[i] = genRandom();
      }
      key[i] = '\0';
      char* buffer;
      buffer = (char*)malloc(strlen(key)+1);
      std::strcpy(buffer, key);
      //Generate the frequency as a random integer between 10 and 4999
      int frequency = rand() % 4000 + 10;
      hashMapForTest[buffer] = frequency;
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
       
       if (getCountOfKey(key) == 0)
       {
         keysToDelete.push_back(key);
       }
       listOfKeys.pop_front();
       ++count;
    }
    while (!keysToDelete.empty())
    {
      KeyType key = keysToDelete.front();
      //While using sparse_hash_map, set_deleted_key should be called before any calls to erase()
      hashMapForTest.set_deleted_key(NULL);
      hashMapForTest.erase(key);
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
      sumTime += timeTaken;
    }
    /*Using Google sparse_hash_map, and with dropping the keys whose frequencies drop to 0, avg time with 3,000 pairs: 0.00724791 seconds. 
      Avg time with 5,000 pairs: 0.0129173 seconds, and avg time with 10,000 pairs: 0.0314402 seconds*/
    cout << "Avg time to update tuples = " << (double)sumTime/(double)nIter << " seconds" << endl;
  }


int main()
{
  FastSpeedTest st;
  st.testUpdateSpeed();
  return 0;
}




   

