                                                      /* Bill Mill and Bryan Brook
                                                         CS371
                                                         Assignment #10
                                                         Due 12/03/03 */

/*
   Source file name: ASGN10.cpp
   Description: This class finds a solution for the Traveling Salesperson
                Problem on the data set given in the handout for problem #9.
                I've included the main() function at the bottom of this file
                because it's so trivial.

                To Compile: Just load ASGN10.cpp into MSVC and click compile

  By Bill Mill and Bryan Brook, 12/02/03
  Written in Viusal C++ 6
*/

#include <iostream>
#include <vector>
#include <string>
using namespace std;

/* this class implements a depth-first search through the city list
    given in the handout for assignment #9 */
class CitySearch {
    public:             //everything's public

    string cities[17];     // The names of all the cities
    int graph[17][17];  // Matrix holding the distances from city to city.
                        // -1 means the cities are not connected
    int visited[17];    // The current list of visited cities
    int visited_dist[17];//The distances to the currently visited cities
    int best[17];       // The currently best tour
    int best_sum;       // The currently best sum
    int level;          // How deep into our tour we are
    int start_city;     // The city from which we want to start
    int end_city;       // The city which we want to end at
    int num_cities;     // The total number of cities, 17 in this example
    int i, j;           // Counter variables

    /* default constructor */
    CitySearch() {
        level = 0;
        start_city = 12;
        end_city = 12;
        num_cities = 17;
        visited_dist[0] = 0;
        best_sum = 0;
        for(i=0; i < num_cities; i++) visited[i] = -1;    //initialize visited
        string city_list[17] = {"Atlanta", "Boston", "Chicago", "Dallas", "Los Angeles", "Las Vegas",
                        "Miami", "Milwaukee", "Minneapolis", "New Orleans", "New York City",
                        "Oklahoma City", "Philadelphia", "Phoenix", "San Francisco",
                        "Seattle", "Washington D.C."};
        for(i=0; i < num_cities; i++) cities[i] = city_list[i];
        int dists[17][17] =
{{0, -1, 585, 717, 1944, 1748, 605, -1, 905, 412, 748, -1, -1, 1589, 2145, 2181, -1}, /*atl*/
{-1, 0, 856, -1, 2605, 2380, -1, -1, -1, -1, -1, 1497, 268, -1, -1, 2496, -1},        /*bos*/
{585, 856, 0, -1, -1, 1780, 1423, -1, -1, 948, -1, -1, -1, -1, -1, 2060, -1},         /*chi*/
{717, -1, -1, 0, 1440, -1, -1, -1, 949, 517, 1614, -1, 1300, -1, -1, -1, -1},         /*dal*/
{1944, 2605, -1, 1440, 0, 272, -1, -1, -1, -1, -1, -1, 2402, -1, 414, -1 -1},         /*la */
{1748, 2380, 1780, -1, 272, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},           /*lv */
{605, -1, 1423, -1, -1, -1, 0, -1, -1, -1, -1, -1, 1024, -1, -1, -1, -1},             /*mmi*/
{-1, -1, -1, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1, 1771, 2257, -1, 811},             /*mil*/
{905, -1, -1, 949, -1, -1, -1, -1, 0, -1, 1217, 792, -1, -1, -1, -1, -1},             /*min*/
{412, -1, 948, 517, -1, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1},               /*no */
{748, -1, -1, 1614, -1, -1, -1, -1, 1217, -1, 0, -1, -1, -1, -1, -1, 237},            /*nyc*/
{-1, 1497, -1, -1, -1, -1, -1, -1, 792, -1, -1, 0, -1, -1, -1, -1, -1},               /*okc*/
{-1, 268, -1, 1300, 2402, -1, 1024, -1, -1, -1, -1, -1, 0, 2081, -1, -1, -1},         /*phi*/
{1589, -1, -1, -1, -1, -1, -1, 1771, -1, -1, -1, -1, 2081, 0, -1, -1, -1},            /*pho*/
{2145, -1, -1, -1, 414, -1, -1, 2257, -1, -1, -1, -1, -1, -1, 0, 808, -1},            /*sfo*/
{2181, 2496, 2060, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 808, 0, -1},           /*sea*/
{-1, -1, -1, -1, -1, -1, -1, 811, -1, -1, 237, -1, -1, -1, -1, -1, 0}};               /*wdc*/
        for(i=0; i < num_cities; i++)
            for(j=0; j < num_cities; j++)
                graph[i][j] = dists[i][j];
    }

    /* search an array for a particular integer
        city: the integer to search for
        visited: the array to search for city in
        sz: the length of the city array */
    bool find(int city, int* visited, int sz) {
        for(int i = 0; i < sz; i++)
            if(visited[i] == city)
                return true;
        return false;
    }

    /* returns the sum of an array.
        dist: an array
        sz:   the length of array dist */
    int sum(int* dist, int sz) {
        int c = 0;
        for(int i = 0; i < sz; i++)
            c += dist[i];
        return c;
    }

    /* recursively search the cities, depth-first.
       city: the number of the row in the graph containing a city's adjacency
              information. */
    void DFSearch(int city) {
        int i = 0;
        vector<int> queue;            //LIFO queue of cities to visit from here

        visited[level] = city;        //add the current city to the visited list

        if(level > 0)       //add the distance from the previous node to this one
            visited_dist[level] = graph[visited[level-1]][city];

        //test for the goal state, see if our tour is the smallest found yet
        if(level == 16 && graph[city][end_city] > 0) {
            cout << ".";                  //let the user know something's happening
            int dist = sum(visited_dist, 17);
            if(dist < best_sum || best_sum == 0) {
                best_sum = dist;
                for (i=0; i < num_cities; i++) { best[i] = visited[i]; }
            }
            //now clean up
            visited[level] = -1;
            visited_dist[level] = -1;
            return;
        }

        //otherwise, queue up the possible cities
        for(i=0; i < num_cities; i++)
            if(graph[city][i] > 0 && !find(i, visited, num_cities))
                queue.push_back(i);

        level++;                          //we're going a level deeper

        for(i=0; i < queue.size(); i++)   //search the queue
            DFSearch(queue[i]);

        level--;                          //now we're done at this level and
        visited[level] = -1;              //below, so clean up
        visited_dist[level] = -1;
    }
}; // end class

int main(void) {
    int i;
    CitySearch cs;

    cs.DFSearch(cs.start_city);
    //print out the best tour
    cout << "\nThe best tour is:\n";
    for(i=0; i < cs.num_cities; i++) cerr << cs.best[i] << " ";
    cout << "12\nWhich is:\n";
    for(i=0; i < cs.num_cities; i++) cerr << cs.cities[cs.best[i]] << " -> ";
    cerr << "Philadelphia\n\n";
    cout << "with a distance of: " << cs.best_sum << "\n\n\n";
    return 0;
}

