#include <math.h>
#include <RcppArmadillo.h>
#include "dftomat.h"
#include <cstddef>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(cpp)]]

// mean radius from:
//  https://nssdc.gsfc.nasa.gov/planetary/factsheet/earthfact.html
#define EARTH_RADIUS_METERS (double)6371000.0

constexpr double DEGS_TO_RADS = (double)M_PI/180.0;
constexpr double DEGS_TO_METERS = EARTH_RADIUS_METERS * DEGS_TO_RADS;
constexpr double METERS_TO_DEGS = 1.0 / DEGS_TO_METERS;
constexpr double DEGS_TO_METERS_SQUARED = pow(DEGS_TO_METERS, 2);

double euclidean_squared(const arma::rowvec& x, const arma::rowvec& y)
{
    // Computes the squared euclidean distance between the two row vectors
    return sum(square(x - y));
}

double manhattan_distance(const arma::rowvec& x, const arma::rowvec& y)
{
    return sum(abs(x - y));
}

double small_haversine_squared(const arma::rowvec& x, const arma::rowvec& y)
{
    auto w = arma::rowvec({ 1, cos((x.at(0) + y.at(0)) / 2) });
    return DEGS_TO_METERS_SQUARED * sum(square((y - x) % w));
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

    for(int i = 0; i < num_rows - 1; ++i)
    {
        arma::rowvec x = c.row(i);

        for(int j = i + 1; j < num_rows; ++j)
        {
            // don't compare a number with itself
            if(check_id && id[i] == id[j])
            {
                continue;
            }

            arma::rowvec y = c.row(j);

            // if the distance is too far even on one dimension, skip the problem
            //if(abs(x[i] - x[j]) > threshold)
            if(arma::as_scalar(abs(x.at(0) - y.at(0))) * (distance_type == "haversine" ? DEGS_TO_METERS : 1.0) > threshold)
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

//' @title rank_of
//' @description Find rank of an element in a sorted array
//' @author Michael Beigelmacher
size_t rank_of(const arma::colvec& vec, const double& x)
{
    size_t left = 0;
    size_t right = vec.size();

    while (left < right) {
        size_t m = (left + right) / 2;
        if (vec.at(m) < x) {
            left = m + 1;
        } else {
            right = m;
        }
    }

    return left;
}

//' @title threshold_distance_compute2
//' @description Computes distance between points in one object to another object
//' @author Michael Beigelmacher
// [[Rcpp::export]]
List threshold_distance2(DataFrame left_obj, DataFrame right_obj, double threshold, CharacterVector cols=CharacterVector("x", "y"), String distance_type = "euclidean")
{
    funcPtr distance_func = nullptr;
    if (distance_type == "euclidean") {
        distance_func = &euclidean_squared;
    } else if (distance_type == "haversine") {
        distance_func = &small_haversine_squared;
    }

    // pre-compute the threshold in squared so we don't need to recompute on every iteration
    const double threshold_squared = pow(threshold, 2);

    arma::mat left_mat = dftomat(left_obj, cols);
    arma::colvec c = left_mat.col(0);
    arma::mat right_mat = dftomat(right_obj, cols);

    // vectors to track indices to keep
    std::vector<int> i_keep;
    std::vector<int> j_keep;
    std::vector<double> distances;

    // keep track of how many we're processing
    R_xlen_t kept = 0;

    for (size_t j = 0; j < right_mat.n_rows; ++j)
    {
        arma::rowvec x = right_mat.row(j);

        for (
            size_t i = rank_of(c, x.at(0) - (distance_type == "haversine" ? METERS_TO_DEGS : 1.0)*threshold);
            i < left_mat.n_rows;
            ++i
            )
        {
            arma::rowvec y = left_mat.row(i);

            // if the distance is too far even on one dimension, skip the problem
            if(arma::as_scalar(abs(x.at(0) - y.at(0))) * (distance_type == "haversine" ? DEGS_TO_METERS : 1.0) > threshold)
            {
                break;
            }

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

    R_xlen_t skipped = right_mat.n_rows * left_mat.n_rows - kept;

    // need to add 1 back to the indices since C++ starts at 0 while R starts at 1
    transform(i_keep.begin(), i_keep.end(), i_keep.begin(), bind2nd(std::plus<int>(), 1));
    transform(j_keep.begin(), j_keep.end(), j_keep.begin(), bind2nd(std::plus<int>(), 1));

    List results = List::create(_["i"]=i_keep, _["j"]=j_keep, _["distance"]=distances, _["kept"]=kept, _["skipped"]=skipped);

    return results;
}
