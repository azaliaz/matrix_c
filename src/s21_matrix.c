#include "s21_matrix.h"

int s21_create_matrix(int rows, int columns, matrix_t *result) {
  int code = OK;
  if (rows > 0 && columns > 0 && result &&
      (result->matrix = (double **)calloc(rows, sizeof(double)))) {
    result->columns = columns;
    result->rows = rows;
    for (int i = 0; i < rows; i++) {
      result->matrix[i] = (double *)calloc(columns, sizeof(double));
      if (result->matrix[i] == NULL) {
        code = INCORRECT_MATRIX;
        for (int j = 0; j < i; j++) {
          free(result->matrix[j]);
        }
        free(result->matrix);
      }
    }
  } else {
    code = INCORRECT_MATRIX;
  }
  return code;
}

void s21_remove_matrix(matrix_t *A) {
  int code = 0;
  if (A == NULL || A->matrix == NULL) {
    code = INCORRECT_MATRIX;
  }
  if (code == OK && A != NULL) {
    for (int i = 0; i < A->rows; i++) {
      if (A->matrix[i] != NULL) {
        free(A->matrix[i]);
      }
    }
    free(A->matrix);
    A->columns = 0;
    A->rows = 0;
    A->matrix = NULL;
  }
}
int s21_matrix_correct(matrix_t *A) {
  int code = OK;
  if (A == NULL)
    code = INCORRECT_MATRIX;
  else if (A->rows <= 0 || A->columns <= 0)
    code = INCORRECT_MATRIX;
  return code;
}
int calc_correct(matrix_t *A) {
  int code = OK;
  if (A == NULL)
    code = INCORRECT_MATRIX;
  else if (A->rows <= 1 || A->columns <= 1) {
    code = INCORRECT_MATRIX;
  }
  return code;
}
int correct_operation(matrix_t *A, matrix_t *B) {
  int code = OK;
  if (A == NULL || B == NULL) {
    code = INCORRECT_MATRIX;
  } else if (A->rows <= 0 || B->rows <= 0 || A->columns <= 0 ||
             B->columns <= 0) {
    code = INCORRECT_MATRIX;
  } else if (A->columns != B->columns || A->rows != B->rows) {
    code = CALC_ERROR;
  }
  return code;
}
int correct_mult_matrix(matrix_t *A, matrix_t *B) {
  int code = OK;
  if (A == NULL || B == NULL) {
    code = INCORRECT_MATRIX;
  } else if (A->rows <= 0 || B->rows <= 0 || A->columns <= 0 ||
             B->columns <= 0) {
    code = INCORRECT_MATRIX;
  } else if (A->columns != B->rows || A->rows != B->columns) {
    code = CALC_ERROR;
  }
  return code;
}
int s21_eq_matrix(matrix_t *A, matrix_t *B) {
  int code = SUCCESS;
  if (A == NULL || B == NULL) {
    code = FAILURE;
  } else if (A->rows <= 0 || B->rows <= 0 || A->columns <= 0 ||
             B->columns <= 0) {
    code = FAILURE;
  } else {
    int row = A->rows;
    int column = A->columns;
    if ((row == B->rows) && (column == B->columns)) {
      for (int i = 0; i < row; i++) {
        for (int j = 0; j < column; j++) {
          if (fabs(A->matrix[i][j] - B->matrix[i][j]) > EPS) {
            code = FAILURE;
          }
        }
      }
    } else {
      code = FAILURE;
    }
  }

  return code;
}
int s21_sum_matrix(matrix_t *A, matrix_t *B, matrix_t *result) {
  int code = OK;
  code = correct_operation(A, B);
  if (code == OK) {
    code = s21_create_matrix(A->rows, A->columns, result);
    if (code == OK) {
      for (int i = 0; i < A->rows; i++) {
        for (int j = 0; j < A->columns; j++) {
          result->matrix[i][j] = A->matrix[i][j] + B->matrix[i][j];
        }
      }
    }
  }
  return code;
}
int s21_sub_matrix(matrix_t *A, matrix_t *B, matrix_t *result) {
  int code = OK;
  code = correct_operation(A, B);
  if (code == OK) {
    code = s21_create_matrix(A->rows, A->columns, result);
    if (code == OK) {
      for (int i = 0; i < A->rows; i++) {
        for (int j = 0; j < A->columns; j++) {
          result->matrix[i][j] = A->matrix[i][j] - B->matrix[i][j];
        }
      }
    }
  }
  return code;
}
int s21_mult_number(matrix_t *A, double number, matrix_t *result) {
  int code = OK;
  code = s21_matrix_correct(A);
  if (code == OK) {
    code = s21_create_matrix(A->rows, A->columns, result);
    if (code == OK) {
      for (int i = 0; i < A->rows; i++) {
        for (int j = 0; j < A->columns; j++) {
          result->matrix[i][j] = A->matrix[i][j] * number;
        }
      }
    }
  }
  return code;
}
int s21_mult_matrix(matrix_t *A, matrix_t *B, matrix_t *result) {
  int code = OK;
  code = correct_mult_matrix(A, B);
  if (code == OK) {
    code = s21_create_matrix(A->rows, B->columns, result);
    if (code == OK) {
      for (int i = 0; i < A->rows; i++) {
        for (int j = 0; j < B->columns; j++) {
          result->matrix[i][j] = 0;
          for (int k = 0; k < B->rows; k++) {
            result->matrix[i][j] += A->matrix[i][k] * B->matrix[k][j];
          }
        }
      }
    }
  }
  return code;
}
int s21_transpose(matrix_t *A, matrix_t *result) {
  int code = OK;
  code = s21_matrix_correct(A);
  if (code == OK) {
    code = s21_create_matrix(A->columns, A->rows, result);
    if (code == OK) {
      for (int i = 0; i < A->columns; i++) {
        for (int j = 0; j < A->rows; j++) {
          result->matrix[i][j] = A->matrix[j][i];
        }
      }
    }
  }
  return code;
}

void s21_minor_of_matrix(matrix_t A, int row, int column, matrix_t *result) {
  int code = OK;
  code = s21_create_matrix(A.rows - 1, A.columns - 1, result);
  if (code == OK) {
    int m = 0;
    int n = 0;
    for (int i = 0; i < A.rows; i++) {
      if (i != row) {
        n = 0;
        for (int j = 0; j < A.columns; j++) {
          if (j != column) {
            result->matrix[m][n] = A.matrix[i][j];
            n++;
          }
        }
        m++;
      }
    }
  }
}
int is_square_matrix(matrix_t *A) {
  int code = OK;
  if (A == NULL) {
    code = INCORRECT_MATRIX;
  } else if (A->rows <= 0 || A->columns <= 0) {
    code = INCORRECT_MATRIX;
  } else if (A->rows != A->columns) {
    code = CALC_ERROR;
  }
  return code;
}
int s21_determinant(matrix_t *A, double *result) {
  int code = OK;
  double d = 0;
  code = s21_matrix_correct(A);
  *result = 0;
  matrix_t C;
  if (result == NULL) code = INCORRECT_MATRIX;
  if (code == OK) {
    if (A->rows != A->columns) {
      code = CALC_ERROR;
    } else {
      if (A->rows == 1) {
        *result = A->matrix[0][0];
      } else if (A->rows == 2) {
        *result = A->matrix[0][0] * A->matrix[1][1] -
                  A->matrix[0][1] * A->matrix[1][0];
      } else {
        for (int i = 0; i < A->rows; i++) {
          s21_minor_of_matrix(*A, 0, i, &C);
          code = s21_determinant(&C, &d);
          *result = *result + A->matrix[0][i] * pow(-1, i) * d;
          s21_remove_matrix(&C);
        }
      }
    }
  }
  return code;
}
int s21_calc_complements(matrix_t *A, matrix_t *result) {
  int code = OK;
  code = s21_matrix_correct(A);
  double det_minor = 0;
  matrix_t minor;
  if (result == NULL) code = INCORRECT_MATRIX;
  if (code == OK) {
    if (A->rows != A->columns) {
      code = CALC_ERROR;
    } else {
      code = s21_create_matrix(A->rows, A->columns, result);
      if (code == OK) {
        if (A->rows > 1) {
          for (int i = 0; i < A->rows; i++) {
            for (int j = 0; j < A->columns; j++) {
              s21_minor_of_matrix(*A, i, j, &minor);
              code = s21_determinant(&minor, &det_minor);
              result->matrix[i][j] = pow(-1, i + j) * det_minor;
              s21_remove_matrix(&minor);
            }
          }
        } else {
          result->matrix[0][0] = 1;
        }
      }
    }
  }
  return code;
}

int s21_inverse_matrix(matrix_t *A, matrix_t *result) {
  int code = OK;
  code = s21_matrix_correct(A);
  matrix_t tmp;
  matrix_t d;
  if (result == NULL) code = INCORRECT_MATRIX;
  if (code == OK) {
    if (A->columns != A->rows) {
      code = CALC_ERROR;
    } else {
      double det = 0;
      code = s21_determinant(A, &det);
      if (code == OK && fabs(det) > EPS) {
        code = s21_calc_complements(A, &d);
        code = s21_transpose(&d, &tmp);
        code = s21_mult_number(&tmp, (1 / det), result);
        s21_remove_matrix(&tmp);
        s21_remove_matrix(&d);
      } else
        code = CALC_ERROR;
    }
  }
  return code;
}
