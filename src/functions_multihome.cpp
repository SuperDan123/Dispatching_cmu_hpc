#include <RcppEigen.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppGSL)]]
#include <iostream>
#include <Eigen/Dense>
#include <RcppGSL.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_multiroots.h>
#include <gsl/gsl_roots.h>
#include <cmath>
#include <stdexcept>
#include <functional>

// [[Rcpp::export]]
Eigen::MatrixXd compute_a_f_tj_rcpp(
  Eigen::VectorXd beta_f,
  Eigen::MatrixXd x_a_f,
  Eigen::VectorXd ea_f
) {
  Eigen::MatrixXd a_f = x_a_f * beta_f + ea_f;
  return(a_f);
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_h_f_tj_rcpp(
  Eigen::VectorXd beta_f,
  double lambda_f,
  Eigen::MatrixXd x_a_f,
  Eigen::VectorXd ea_f,
  Eigen::VectorXd f
) {
  Eigen::VectorXd a_f =
    compute_a_f_tj_rcpp(
      beta_f,
      x_a_f,
      ea_f
    );
  Eigen::MatrixXd h_f =
    (a_f + f * lambda_f).array().exp();
  return(h_f);
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_condition_s_f_numerator_tj_rcpp(
  double m_w,
  double m_f,
  Eigen::VectorXd beta_f,
  double lambda_f,
  Eigen::MatrixXd x_a_f,
  double size_w,
  Eigen::VectorXd mu,
  Eigen::VectorXd ea_f,
  Eigen::VectorXd f
) {
  Eigen::MatrixXd h_f =
    compute_h_f_tj_rcpp(
      beta_f,
      lambda_f,
      x_a_f,
      ea_f,
      f
    );
  Eigen::MatrixXd numerator =
    std::pow(size_w, m_w) * mu.array() * h_f.array();
  numerator = 
    numerator.array().pow(1 / (2 - m_f)).matrix();
  return(numerator);
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_condition_s_f_denominator_tj_rcpp(
  double m_w,
  double m_f,
  Eigen::VectorXd beta_f,
  double lambda_f,
  Eigen::MatrixXd x_a_f,
  double size_w,
  double size_f,
  Eigen::VectorXd mu,
  Eigen::VectorXd ea_f,
  Eigen::VectorXd f,
  Eigen::VectorXd s_f
) {
  Eigen::MatrixXd h_f =
    compute_h_f_tj_rcpp(
      beta_f,
      lambda_f,
      x_a_f,
      ea_f,
      f
    );
  Eigen::MatrixXd denominator =
    s_f.array().pow(1 - m_f) * 
    std::pow(size_f, 1 - m_f) * 
    (mu.array() * std::pow(size_w, m_w) * h_f.array()).pow(- (1 - m_f) / (2 - m_f)); 
  denominator = 
    (
      denominator.array() +
        (
          mu.array() * std::pow(size_w, m_w) * h_f.array()
        ).pow(1 / (2 - m_f)).sum()
    ).matrix();
  return(denominator);
  }

// [[Rcpp::export]]
Eigen::MatrixXd compute_condition_s_f_tj_rcpp(
  double m_w,
  double m_f,
  Eigen::VectorXd beta_f,
  double lambda_f,
  Eigen::MatrixXd x_a_f,
  double size_w,
  double size_f,
  Eigen::VectorXd mu,
  Eigen::VectorXd ea_f,
  Eigen::VectorXd f,
  Eigen::VectorXd s_f
) {
  Eigen::VectorXd numerator =
    compute_condition_s_f_numerator_tj_rcpp(
      m_w,
      m_f,
      beta_f,
      lambda_f,
      x_a_f,
      size_w,
      mu,
      ea_f,
      f
    );
  Eigen::VectorXd denominator =
    compute_condition_s_f_denominator_tj_rcpp(
      m_w,
      m_f,
      beta_f,
      lambda_f,
      x_a_f,
      size_w,
      size_f,
      mu,
      ea_f,
      f,
      s_f
    );
  Eigen::MatrixXd condition =
    s_f.array() - numerator.array() / denominator.array();
  return(condition);
}

struct solver_params {
  int n;
  double m_w;
  double m_f;
  Eigen::VectorXd beta_f;
  double lambda_f;
  Eigen::MatrixXd x_a;
  double size_w;
  double size_f;
  Eigen::VectorXd mu;
  Eigen::VectorXd ea_f;
  Eigen::VectorXd f;
};

int fn(const gsl_vector *x, void *params, gsl_vector *f) {
  solver_params *p = static_cast<solver_params *>(params);
  int n = p->n;
  Eigen::VectorXd exp_x(n);
  
  for (int i = 0; i < n; ++i) {
    exp_x(i) = exp(gsl_vector_get(x, i));
  }
  
  Eigen::VectorXd s_f_x = exp_x / (1 + exp_x.sum());
  Eigen::MatrixXd condition_t = compute_condition_s_f_tj_rcpp(
    p->m_w, p->m_f, p->beta_f, p->lambda_f, p->x_a, p->size_w, p->size_f, p->mu, p->ea_f, p->f, s_f_x
  );
  
  for (int i = 0; i < n; ++i) {
    gsl_vector_set(f, i, condition_t(i, 0));
  }
  
  return GSL_SUCCESS;
}

// [[Rcpp::export]]
Eigen::MatrixXd solve_s_f_tj_rcpp(
    double m_w,
    double m_f,
    Eigen::VectorXd beta_f,
    double lambda_f,
    Eigen::MatrixXd x_a_f,
    double size_w,
    double size_f,
    Eigen::VectorXd mu,
    Eigen::VectorXd ea_f,
    Eigen::VectorXd f,
    Eigen::VectorXd s_f
) {
  int n = s_f.size();
  gsl_vector *x = gsl_vector_calloc(n);
  
  solver_params params = {n, m_w, m_f, beta_f, lambda_f, x_a_f, size_w, size_f, mu, ea_f, f};
  
  // Set up GSL solver
  const gsl_multiroot_fsolver_type *T;
  gsl_multiroot_fsolver *s;
  gsl_multiroot_function F;
  F.f = &fn;
  F.n = n;
  F.params = &params;
  
  T = gsl_multiroot_fsolver_hybrids;
  s = gsl_multiroot_fsolver_alloc(T, n);
  
  // Initialize the solver
  gsl_multiroot_fsolver_set(s, &F, x);
  
  // Iterate to find the root
  int status;
  int iter = 0, max_iter = 1000;
  do {
    iter++;
    status = gsl_multiroot_fsolver_iterate(s);
    
    if (status) {
      break;
    }
    
    status = gsl_multiroot_test_residual(s->f, 1e-7);
    
  } while (status == GSL_CONTINUE && iter < max_iter);
  
  Eigen::MatrixXd solution_x(n, 1);
  for (int i = 0; i < n; ++i) {
    solution_x(i, 0) = gsl_vector_get(s->x, i);
  }
  
  gsl_multiroot_fsolver_free(s);
  gsl_vector_free(x);
  
  solution_x = 
    (solution_x.array().exp() / 
    (1 + solution_x.array().exp().sum())).matrix();
  
  return solution_x;
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_a_w_tj_rcpp(
  const Eigen::VectorXd& beta_w,
  const Eigen::MatrixXd& x_a_w,
  const Eigen::MatrixXd& ea_w
) {
  Eigen::MatrixXd a_w = x_a_w * beta_w + ea_w;
  return a_w;
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_h_w_tj_rcpp(
  const Eigen::MatrixXd& a_w,
  double lambda_w,
  const Eigen::VectorXd& w
) {
  Eigen::MatrixXd h_w = (a_w.array() + w.array() * lambda_w).exp();
  return h_w;
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_meeting_number_tj_rcpp(
  double m_w,
  double m_f,
  double size_w,
  double size_f,
  const Eigen::VectorXd& mu,
  const Eigen::VectorXd& s_f
) {
  Eigen::MatrixXd q = mu.array() * std::pow(size_w, m_w) * std::pow(size_f, m_f) * s_f.array().pow(m_f);
  return q;
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_meeting_probability_w_tj_rcpp(
  double m_w,
  double m_f,
  double size_w,
  double size_f,
  const Eigen::VectorXd& mu,
  const Eigen::VectorXd& s_f
) {
  Eigen::MatrixXd q = compute_meeting_number_tj_rcpp(
    m_w,
    m_f, 
    size_w,
    size_f,
    mu,
    s_f
  );
  
  Eigen::MatrixXd p = q.array() / size_w;
  return p;
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_omega_tj_rcpp(
  const Eigen::MatrixXd& meeting_probability_w,
  const Eigen::VectorXi& met
) {
  // Get probabilities for met and unmet firms
  Eigen::VectorXd p_met(met.size());
  for(int i = 0; i < met.size(); i++) {
    p_met(i) = meeting_probability_w(met(i)-1); // -1 for 0-based indexing
  }
  
  // Find unmet indices
  Eigen::VectorXi unmet(meeting_probability_w.rows());
  int unmet_count = 0;
  for(int i = 0; i < meeting_probability_w.rows(); i++) {
    if((met.array() == i+1).count() == 0) { // Adjust for 1-based indexing
      unmet(unmet_count++) = i;
    }
  }
  unmet.conservativeResize(unmet_count);
  
  Eigen::VectorXd p_unmet(unmet.size());
  for(int i = 0; i < unmet.size(); i++) {
    p_unmet(i) = meeting_probability_w(unmet(i));
  }

  // Calculate omega_f
  double omega_f_val = 1.0;
  for(int i = 0; i < p_met.size(); i++) {
    omega_f_val *= p_met(i);
  }
  for(int i = 0; i < p_unmet.size(); i++) {
    omega_f_val *= (1.0 - p_unmet(i));
  }

  // Create result vector and handle small probabilities
  Eigen::MatrixXd omega_f = meeting_probability_w;
  for(int i = 0; i < omega_f.rows(); i++) {
    if(omega_f(i) < 1e-16) {
      omega_f(i) = 1e-16;
    }
  }
  
  // Divide by meeting probabilities and zero out unmet
  omega_f = omega_f_val / omega_f.array();
  
  for(int i = 0; i < unmet.size(); i++) {
    omega_f(unmet(i)) = 0;
  }

  return omega_f;
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_rho_tj_rcpp(
  const Eigen::VectorXd& h_w,
  const Eigen::VectorXd& meeting_probability_w,
  const Eigen::VectorXi& met
) {
  // Find unmet indices
  Eigen::VectorXi unmet(meeting_probability_w.size());
  int unmet_count = 0;
  for(int i = 0; i < meeting_probability_w.size(); i++) {
    if((met.array() == i+1).count() == 0) { // Adjust for 1-based indexing
      unmet(unmet_count++) = i;
    }
  }
  unmet.conservativeResize(unmet_count);

  // Create h_w_met and zero out unmet entries
  Eigen::VectorXd h_w_met = h_w;
  for(int i = 0; i < unmet.size(); i++) {
    h_w_met(unmet(i)) = 0;
  }

  // Calculate rho_f
  double sum_h_w_met = h_w_met.sum();
  Eigen::MatrixXd rho_f = h_w_met / (1.0 + sum_h_w_met);

  return rho_f;
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_s_w_met_rcpp(
  Eigen::VectorXd meeting_probability_w,
  Eigen::VectorXd h_w,
  Eigen::VectorXi met
) {
  // Compute omega
  Eigen::MatrixXd omega = compute_omega_tj_rcpp(
    meeting_probability_w,
    met
  );

  // Compute rho
  Eigen::MatrixXd rho = compute_rho_tj_rcpp(
    h_w,
    meeting_probability_w, 
    met
  );

  // Element-wise multiplication
  Eigen::MatrixXd s_w_met = omega.array() * rho.array();

  return s_w_met;
}

// [[Rcpp::export]]
Eigen::VectorXd solve_s_w_tj_from_a_w_exact_rcpp(
  Eigen::VectorXd a_w,
  double m_w,
  double m_f,
  double lambda_w,
  double size_w,
  double size_f,
  Eigen::VectorXd mu,
  Eigen::VectorXd w,
  Eigen::VectorXd s_f
) {
  // Compute h_w
  Eigen::VectorXd h_w = compute_h_w_tj_rcpp(a_w, lambda_w, w);
  
  // Compute meeting probability
  Eigen::VectorXd meeting_probability_w = compute_meeting_probability_w_tj_rcpp(
    m_w, m_f, size_w, size_f, mu, s_f
  );

  // Generate power set of indices
  int n = s_f.size();
  
  // Restrict n to prevent exponential blow-up
  if(n > 20) { // Adjust the threshold as needed
    throw std::runtime_error("s_f size is too large to handle with an exact method.");
  }
  
  std::vector<Eigen::VectorXi> met_list;
  unsigned long long total_subsets = 1ULL << n; // Use 64-bit shifts
  met_list.reserve(total_subsets); // Optional: reserve memory upfront
  
  for(unsigned long long i = 0; i < total_subsets; i++) {
    std::vector<int> subset;
    for(int j = 0; j < n; j++) {
      if(i & (1ULL << j)) {
        subset.push_back(j + 1); // 1-based indexing
      }
    }
    
    Eigen::VectorXi met(subset.size());
    if(!subset.empty()) {
      for(int k = 0; k < subset.size(); k++) {
        met(k) = subset[k];
      }
    }
    met_list.emplace_back(met);
  }

  // Initialize result vector
  Eigen::VectorXd s_w = Eigen::VectorXd::Zero(s_f.size());
  
  // Sum over all possible meeting combinations
  for(const auto& met : met_list) {
    Eigen::MatrixXd s_w_met = compute_s_w_met_rcpp(
      meeting_probability_w,
      h_w,
      met
    );
    s_w += s_w_met;
  }

  return s_w;
}

// [[Rcpp::export]]
Rcpp::List compute_h_w_meetng_probability_full_rcpp(
  Eigen::VectorXd a_w,
  double m_w,
  double m_f,
  double lambda_w,
  double size_w,
  double size_f,
  Eigen::VectorXd mu,
  Eigen::VectorXd w,
  Eigen::VectorXd s_f
) {
  // Compute h_w using existing function
  Eigen::VectorXd h_w = compute_h_w_tj_rcpp(a_w, lambda_w, w);
  
  // Create h_w_full by prepending 1
  Eigen::VectorXd h_w_full(h_w.size() + 1);
  h_w_full(0) = 1.0;
  h_w_full.tail(h_w.size()) = h_w;

  // Compute meeting probability using existing function  
  Eigen::VectorXd meeting_probability_w = compute_meeting_probability_w_tj_rcpp(
    m_w, m_f, size_w, size_f, mu, s_f
  );

  // Create meeting_probability_w_full by prepending 1
  Eigen::VectorXd meeting_probability_w_full(meeting_probability_w.size() + 1);
  meeting_probability_w_full(0) = 1.0;
  meeting_probability_w_full.tail(meeting_probability_w.size()) = meeting_probability_w;

  // Return as list
  return Rcpp::List::create(
    Rcpp::Named("h_w_full") = h_w_full,
    Rcpp::Named("meeting_probability_w_full") = meeting_probability_w_full
  );
}

double pgev(
    double x,
    double mu,
    double sigma, 
    double xi
) {
    if (sigma <= 0) {
        throw std::invalid_argument("Scale parameter sigma must be positive.");
    }

    double z = (x - mu) / sigma;
    if (xi == 0) {
        return exp(-exp(-z)); // Gumbel distribution
    } else {
        double t = 1 + xi * z;
        if (t <= 0) {
            return 0.0; // Return 0 if the argument is out of the domain
        }
        return exp(-pow(t, -1 / xi));
    }
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_utility_distribution_rcpp(
    double u, 
    const Eigen::VectorXd& utility
) {
    Eigen::MatrixXd distribution(utility.size(), 1);
    
    for(int i = 0; i < utility.size(); ++i) {
        // Parameters for GEV: mu = utility[i], sigma = 1, xi = 0 (Gumbel)
        try {
            distribution(i, 0) = pgev(u, utility(i), 1.0, 0.0);
        } catch(const std::invalid_argument& e) {
            Rcpp::stop(e.what());
        }
    }
    
    return distribution;
}

double dgev(
  double x, 
  double mu, 
  double sigma, 
  double xi
) {
    if (sigma <= 0) {
        throw std::invalid_argument("Scale parameter sigma must be positive.");
    }

    double z = (x - mu) / sigma;
    if (xi == 0) {
        double exp_z = exp(-z);
        return (1.0/sigma) * exp_z * exp(-exp_z); // Gumbel density
    } else {
        double t = 1 + xi * z;
        if (t <= 0) {
            return 0.0;
        }
        return (1.0/sigma) * pow(t, -1/xi - 1) * exp(-pow(t, -1/xi));
    }
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_utility_density_rcpp(
    double u,
    const Eigen::VectorXd& utility
) {
    Eigen::MatrixXd density(utility.size(), 1);
    
    for(int i = 0; i < utility.size(); ++i) {
        // Parameters for GEV: mu = utility[i], sigma = 1, xi = 0 (Gumbel)
        try {
            density(i, 0) = dgev(u, utility(i), 1.0, 0.0);
        } catch(const std::invalid_argument& e) {
            Rcpp::stop(e.what());
        }
    }
    
    return density;
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_integrand_rcpp(
    double u,
    Eigen::VectorXd utility,
    Eigen::VectorXd consideration
) {
    // compute density of utility
    Eigen::VectorXd density = compute_utility_density_rcpp(u, utility);
    
    // compute distribution of utility 
    Eigen::VectorXd distribution = compute_utility_distribution_rcpp(u, utility);
    
    // compute components
    Eigen::VectorXd components = (1.0 - consideration.array() + 
                                consideration.array() * distribution.array()).matrix();
    
    // threshold small values
    for(int i = 0; i < components.size(); i++) {
        if(components(i) < 1e-16) {
            components(i) = 1e-16;
        }
    }
    
    // compute product
    double product = components.prod();
    
    // create product matrix
    Eigen::VectorXd product_vec = Eigen::VectorXd::Constant(components.size(), product);
    
    // divide by components
    product_vec = product_vec.array() / components.array();
    
    // compute integrand
    Eigen::MatrixXd integrand = (product_vec.array() * density.array()).matrix();
    
    return integrand;
}

// [[Rcpp::export]]
Eigen::VectorXd compute_f_upper_rcpp(
    double u,
    Eigen::VectorXd utility,
    double margin
) {
    double utility_max = utility.maxCoeff();
    Eigen::VectorXd distribution = compute_utility_distribution_rcpp(u, Eigen::VectorXd::Constant(1, utility_max));
    Eigen::VectorXd y = distribution.array() - (1.0 - margin/2.0);
    return y;
}

// [[Rcpp::export]]
Eigen::VectorXd compute_f_lower_rcpp(
    double u,
    Eigen::VectorXd utility,
    double margin
) {
    double utility_min = utility.minCoeff();
    Eigen::VectorXd distribution = compute_utility_distribution_rcpp(u, Eigen::VectorXd::Constant(1, utility_min));
    Eigen::VectorXd y = distribution.array() - margin/2.0;
    return y;
}

// Root finding function using GSL
double find_root(std::function<Eigen::VectorXd(double, Eigen::VectorXd, double)> f,
                double a, double b,
                Eigen::VectorXd utility,
                double margin,
                double tol) {
    gsl_function F;
    struct root_params {
        std::function<Eigen::VectorXd(double, Eigen::VectorXd, double)> f;
        Eigen::VectorXd utility;
        double margin;
    };
    
    root_params params = {f, utility, margin};
    
    auto wrapper = [](double x, void* params) {
        root_params* p = static_cast<root_params*>(params);
        return p->f(x, p->utility, p->margin)(0);
    };
    
    F.function = wrapper;
    F.params = &params;
    
    // Check if endpoints bracket a root
    double fa = wrapper(a, &params);
    double fb = wrapper(b, &params);
    
    // If endpoints don't bracket a root, expand the interval
    int max_expansions = 10;
    int expansions = 0;
    double expansion_factor = 1.5;
    
    while (fa * fb > 0 && expansions < max_expansions) {
        a -= (b - a) * expansion_factor;
        b += (b - a) * expansion_factor;
        fa = wrapper(a, &params);
        fb = wrapper(b, &params);
        expansions++;
    }
    
    if (fa * fb > 0) {
        Rcpp::stop("Could not bracket root even after expanding search interval");
    }
    
    const gsl_root_fsolver_type *T = gsl_root_fsolver_brent;
    gsl_root_fsolver *s = gsl_root_fsolver_alloc(T);
    
    int status = gsl_root_fsolver_set(s, &F, a, b);
    if (status != GSL_SUCCESS) {
        gsl_root_fsolver_free(s);
        Rcpp::stop("Failed to initialize root finder");
    }
    
    int iter = 0;
    const int max_iter = 1000;  // Increased from 100
    double r = 0;
    
    do {
        iter++;
        status = gsl_root_fsolver_iterate(s);
        if (status != GSL_SUCCESS) break;
        
        r = gsl_root_fsolver_root(s);
        double x_lo = gsl_root_fsolver_x_lower(s);
        double x_hi = gsl_root_fsolver_x_upper(s);
        status = gsl_root_test_interval(x_lo, x_hi, tol, 0.0);
        
        // Add convergence check
        if (fabs(wrapper(r, &params)) < tol) {
            status = GSL_SUCCESS;
            break;
        }
    } while (status == GSL_CONTINUE && iter < max_iter);
    
    gsl_root_fsolver_free(s);
    
    if (status != GSL_SUCCESS) {
        Rcpp::warning("Root finding did not achieve desired tolerance. Using best estimate.");
    }
    
    return r;
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_choice_probability_with_consideration_rcpp(
    Eigen::VectorXd utility,
    Eigen::VectorXd consideration,
    double margin,
    int quadrature_size,
    double tol
) {
    double a = utility.minCoeff() - 30;
    double b = utility.maxCoeff() + 30;

    // Find upper and lower bounds using GSL root finder
    double u_upper = find_root(compute_f_upper_rcpp, a, b, utility, margin, tol);
    double u_lower = find_root(compute_f_lower_rcpp, a, b, utility, margin, tol);

    // Set quadrature points
    Eigen::VectorXd u_sequence = Eigen::VectorXd::LinSpaced(quadrature_size, u_lower, u_upper);

    // Initialize result vector
    Eigen::MatrixXd result = Eigen::MatrixXd::Zero(utility.size(), 1);

    // Compute G at each quadrature point and accumulate results
    for(int n = 0; n < u_sequence.size()-1; n++) {
        double u_n = u_sequence(n);
        double u_n_1 = u_sequence(n+1);
        Eigen::MatrixXd G_n = compute_integrand_rcpp(u_n, utility, consideration);
        Eigen::MatrixXd G_n_1 = compute_integrand_rcpp(u_n_1, utility, consideration);
        Eigen::MatrixXd G_n_bar = (G_n + G_n_1) / 2.0;
        result += G_n_bar * (u_n_1 - u_n);
    }

    return result;
}

// [[Rcpp::export]]
Eigen::MatrixXd solve_s_w_tj_from_a_w_approximate_rcpp(
  Eigen::VectorXd a_w,
  double m_w,
  double m_f,
  double lambda_w,
  double size_w,
  double size_f,
  Eigen::VectorXd mu,
  Eigen::VectorXd w,
  Eigen::VectorXd s_f,
  double margin,
  int quadrature_size,
  double tol
) {
  // Get h_w and meeting probabilities using the helper function
  Rcpp::List full = compute_h_w_meetng_probability_full_rcpp(
    a_w, m_w, m_f, lambda_w, size_w, size_f, mu, w, s_f
  );
  
  // Extract h_w_full and meeting_probability_w_full
  Eigen::VectorXd h_w_full = full["h_w_full"];
  Eigen::VectorXd meeting_probability_w_full = full["meeting_probability_w_full"];
  
  // Fix: sanitize before log
  for (int i = 0; i < h_w_full.size(); ++i) {
    if (!std::isfinite(h_w_full(i)) || h_w_full(i) <= 0.0) {
      h_w_full(i) = 1e-20;  // Lower bound
    } else if (h_w_full(i) > 1e20) {
      h_w_full(i) = 1e20;   // Upper bound
    }
  }
  
  // Compute choice probability with consideration
  Eigen::MatrixXd s_w_full = 
    compute_choice_probability_with_consideration_rcpp(
      h_w_full.array().log().matrix(),
      meeting_probability_w_full,
      margin,
      quadrature_size,
      tol
    );
    
  // Drop first row (outside option)
  Eigen::MatrixXd s_w = s_w_full.bottomRows(s_w_full.rows() - 1);
  
  return s_w;
}


// [[Rcpp::export]]
Eigen::MatrixXd compute_demand_shock_a_f_tj_rcpp(
  double m_f,
  double m_w, 
  double lambda_f,
  Eigen::VectorXd mu,
  double size_f,
  double size_w,
  Eigen::VectorXd f,
  Eigen::VectorXd s_f
) {
  double s_f_0 = 1.0 - s_f.sum();
  
  Eigen::MatrixXd a_f = s_f.array() / 
    (s_f_0 * (mu.array() * std::pow(size_w, m_w)) * 
     (size_f * s_f.array()).pow(m_f - 1));
    
  a_f = a_f.array().log() - lambda_f * f.array();
  
  return a_f;
}

struct objective_params {
    double m_w;
    double m_f; 
    double lambda_w;
    double size_w;
    double size_f;
    Eigen::VectorXd mu;
    Eigen::VectorXd w;
    Eigen::VectorXd s_f;
    Eigen::VectorXd s_w_real;
    std::string method_s_w;
    double margin;
    double quadrature_size;
    double tol;
};

// Then modify the objective function to be a regular function instead of a lambda
int objective_fn(const gsl_vector *x, void *params, gsl_vector *f) {
    objective_params *p = (objective_params *)params;
    
    Eigen::VectorXd a_w(p->w.size());
    for(int i = 0; i < p->w.size(); i++) {
        a_w(i) = gsl_vector_get(x, i);
    }

    Eigen::VectorXd s_w;
      if (p->method_s_w == "exact") {
        s_w = solve_s_w_tj_from_a_w_exact_rcpp(
          a_w, p->m_w, p->m_f, p->lambda_w, p->size_w, p->size_f,
          p->mu, p->w, p->s_f
        );
      } else {
        s_w = solve_s_w_tj_from_a_w_approximate_rcpp(
          a_w, p->m_w, p->m_f, p->lambda_w, p->size_w, p->size_f,
          p->mu, p->w, p->s_f, p->margin, p->quadrature_size, p->tol
        );
      }


    for (int i = 0; i < p->w.size(); i++) {
      double penalty = 0.0;
      gsl_vector_set(f, i, log(p->s_w_real(i)) - log(s_w(i)) + penalty);
  }

    return GSL_SUCCESS;
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_demand_shock_nleqslv_a_w_tj_rcpp(
  double m_w,
  double m_f,
  double lambda_w,
  double size_w,
  double size_f,
  Eigen::VectorXd mu,
  Eigen::VectorXd w,
  Eigen::VectorXd s_f,
  Eigen::VectorXd s_w,
  std::string method_s_w,
  double margin,
  double quadrature_size,
  double tol
) {
  // Store original s_w for comparison
  Eigen::VectorXd s_w_real = s_w;

  // Initialize solver parameters
  const size_t n = w.size();
  const gsl_multiroot_fsolver_type *T = gsl_multiroot_fsolver_hybrids;
  gsl_multiroot_fsolver *solver = gsl_multiroot_fsolver_alloc(T, n);

  // Create struct to hold parameters needed by objective function
  objective_params params = {
    m_w, m_f, lambda_w, size_w, size_f, mu, w, s_f, s_w_real,
    method_s_w, margin, quadrature_size, tol
  };

  // Define objective function
  gsl_multiroot_function F;
  F.f = &objective_fn;
  F.n = n;
  F.params = &params;

  // Declare and initialize x
  gsl_vector *x = gsl_vector_calloc(n);

  gsl_multiroot_fsolver_set(solver, &F, x);

  // Iterate to solve
  int status;
  int iter = 0;
  const int max_iter = 1000;
  
  do {
    iter++;
    status = gsl_multiroot_fsolver_iterate(solver);
    
    if(status) break;
    
    status = gsl_multiroot_test_residual(solver->f, 1e-7);
  } while(status == GSL_CONTINUE && iter < max_iter);

  // Extract solution
  Eigen::MatrixXd a_w(n, 1);
  for(size_t i = 0; i < n; i++) {
    a_w(i,0) = gsl_vector_get(solver->x, i);
  }

  // Clean up
  gsl_multiroot_fsolver_free(solver);
  gsl_vector_free(x);

  return a_w;
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_demand_shock_iteration_a_w_tj_rcpp(
  double m_w,
  double m_f, 
  double lambda_w,
  double size_w,
  double size_f,
  Eigen::VectorXd mu,
  Eigen::VectorXd w,
  Eigen::VectorXd s_f,
  Eigen::VectorXd s_w,
  std::string method_s_w,
  double margin,
  int quadrature_size,
  double tol
) {
  Eigen::VectorXd s_w_real = s_w;
  
  // Initialize old_a_w
  Eigen::MatrixXd old_a_w = Eigen::MatrixXd::Constant(w.size(), 1, 0.1);
  
  double distance = 100.0;
  
  while (distance > 1e-10) {
    Eigen::VectorXd s_w_calc;
    
    if (method_s_w == "exact") {
      s_w_calc = solve_s_w_tj_from_a_w_exact_rcpp(
        old_a_w,
        m_w,
        m_f,
        lambda_w,
        size_w,
        size_f,
        mu,
        w,
        s_f
      );
    } else {
      s_w_calc = solve_s_w_tj_from_a_w_approximate_rcpp(
        old_a_w,
        m_w,
        m_f,
        lambda_w,
        size_w,
        size_f,
        mu,
        w,
        s_f,
        margin,
        quadrature_size,
        tol
      );
    }
    
    Eigen::MatrixXd objective = (s_w_real.array().log() - s_w_calc.array().log()).matrix();
    objective.resize(w.size(), 1);
    
    Eigen::MatrixXd a_w = old_a_w + objective;
    
    distance = (old_a_w - a_w).cwiseAbs().maxCoeff();
    old_a_w = a_w;
  }
  
  return old_a_w;
}

