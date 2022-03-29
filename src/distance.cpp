#include <RcppArmadillo.h>
#include "dftomat.h"
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(cpp)]]

// mean radius from:
//  https://nssdc.gsfc.nasa.gov/planetary/factsheet/earthfact.html
#define EARTH_RADIUS (double)6371000.0

constexpr double EARTH_RADIUS_SQUARED = pow(EARTH_RADIUS, 2);

double euclidean_squared(const arma::rowvec& x, const arma::rowvec& y)
{
    // Computes the squared euclidean distance between the two row vectors
    return sum(square(x - y));
}

double small_haversine_squared(const arma::rowvec& x, const arma::rowvec& y)
{
    auto w = arma::rowvec({ 1, cos((x.at(0) + y.at(0)) / 2) });
    return EARTH_RADIUS_SQUARED * sum(square((y - x) % w));
}

typedef double (*funcPtr)(const arma::rowvec&, const arma::rowvec&);

//' @title threshold_distance_compute
//' @description Computes pairwise distances
//' @author Jared P. Lander
// [[Rcpp::export]]
List threshold_distance(DataFrame obj, double threshold, CharacterVector cols=CharacterVector("x", "y"), String id_col="ID", bool check_id=TRUE, String distance_type = "euclidean")
{
    funcPtr distance_func = nullptr;
    if (distance_type == "euclidean") {
        distance_func = &euclidean_squared;
    } else if (distance_type == "haversine") {
        distance_func = &small_haversine_squared;
    }

    // pre-compute the threshold in squared so we don't need to recompute on every iteration
    const double threshold_squared = pow(threshold, 2);

    int num_rows = obj.nrow();
    // get x/y vectors
    //arma::vec x = obj[x_col];
    //arma::vec y = obj[y_col];
    // combine into a matrix
    //arma::mat c = arma::join_rows(x, y);
    arma::mat c = dftomat(obj, cols);

    IntegerVector id;
    if(check_id)
    {
        id = obj[id_col];
    }
    else
    {
        id.push_back(0);
    }

    // vectors to track indices to keep
    std::vector<int> i_keep;
    std::vector<int> j_keep;
    std::vector<double> distances;

    // keep track of how many we're processing
    R_xlen_t kept = 0;

    for(int i = 0; i < num_rows; ++i)
    {
        arma::rowvec x = c.row(i);

        for(int j = i; j < num_rows; ++j)
        {
            arma::rowvec y = c.row(j);

            // don't compare a number with itself
            if(i == j || (check_id && id[i] == id[j]))
            {
                continue;
            }

            // if the distance is too far even on one dimension, skip the problem
            //if(abs(x[i] - x[j]) > threshold)
            if(arma::as_scalar(abs(x.at(0) - y.at(0))) * (distance_type == "haversine" ? EARTH_RADIUS : 1.0) > threshold)
            {
                break;
            }

            // if the distance is too far on the unsorted dimension, skip the point
            //if (abs(y[i] - y[j]) > threshold)
            //{
            //    continue;
            //}

            // compute the distance
            double the_dist_squared = distance_func(x, y);

            // if this distance is too big, skip ahead
            //if(the_dist > threshold)
            if(the_dist_squared > threshold_squared)
            {
                continue;
            }

            // keep track of which entries were right
            i_keep.push_back(i);
            j_keep.push_back(j);
            //distances.push_back(the_dist);
            distances.push_back(sqrt(the_dist_squared));
            kept++;
        }
    }

    //R_xlen_t kept = i_keep.size();
    R_xlen_t skipped = num_rows*(num_rows - 1)/2 - kept;

    // need to add 1 back to the indices since C++ starts at 0 while R starts at 1
    transform(i_keep.begin(), i_keep.end(), i_keep.begin(), bind2nd(std::plus<int>(), 1));
    transform(j_keep.begin(), j_keep.end(), j_keep.begin(), bind2nd(std::plus<int>(), 1));

    List results = List::create(_["i"]=i_keep, _["j"]=j_keep, _["distance"]=distances, _["kept"]=kept, _["skipped"]=skipped);

    return results;
}
