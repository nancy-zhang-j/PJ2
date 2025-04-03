#include <stdio.h>
#include <stdlib.h>
#include <string.h>


//functions for multiplciation, transpose, inverse, allocation, deallocatoin, initialize unit matrix
//testing function 

//choose a representation for matrices, example below was given in class

// struct matrix { //one idea
//     unsigned rows;
//     unsigned cols;
//     double **data;
// };

// allocate all matricies in the beginning
// multiply(matrix *out, matrix *a, matrix *b) //example for multiply function
//transpose(matrix *out, matrix *in)

// matrix structure definition
typedef struct matrix{
    int rows;
    int cols;
    double **data;
} matrix;

//allocate for a matrix given rows and columns
matrix *newMatrix(int rows, int cols) 
{
    matrix *mat = (matrix *)malloc(sizeof(matrix));
    
    if (!mat) 
    {
        exit(EXIT_FAILURE);
    }

    mat->rows = rows; //set rows
    mat->cols = cols; //set cols
    mat->data = (double **)malloc(rows * sizeof(double *)); //outside matrix

    if (!mat->data) 
    {        
        free(mat);
        exit(EXIT_FAILURE);
    }

    for (int i = 0; i < rows; i++) 
    {
        mat->data[i] = (double *)malloc(cols * sizeof(double)); //inner matrix

        if (!mat->data[i]) 
        {
            for (int j = 0; j < i; j++) 
            {
                free(mat->data[j]);
            }
            free(mat->data);
            free(mat);
            exit(EXIT_FAILURE);
        }
    }
    return mat;
}

//deallocate matrix
void freeMatrix(matrix *mat) 
{
    if (mat != NULL) 
    {
        for (int i = 0; i < mat->rows; i++) 
        {
            free(mat->data[i]);
        }
        free(mat->data);
        free(mat);
    }
}

int multiply(matrix *mat1, matrix *mat2, matrix *result) //combine later, checked if 0 causes error
{
    if(mat1 == NULL || mat2 == NULL || result == NULL || mat1->cols != mat2->rows || result->rows != mat1->rows || result->cols !=mat2->cols) //not able to be multiplied or one is null
    {
        return EXIT_FAILURE;
    }

    for (int i = 0; i < mat1->rows; i++) //mult
    {
        for (int j = 0; j < mat2->cols; j++) 
        {
            result->data[i][j] = 0.0; //initialize to zero or else error
            for (int k = 0; k < mat1->cols; k++) 
            {
                result->data[i][j] += mat1->data[i][k] * mat2->data[k][j]; //pointers due to struct
            }
        }
    }

    return EXIT_SUCCESS;
}

int transpose(matrix *mat1, matrix *result) //just switch the rows and columsn
{
    if(mat1 == NULL || result == NULL || result->rows != mat1->cols || result->cols != mat1->rows)
    {
        return EXIT_FAILURE;
    }
    for (int i = 0; i < mat1->rows; i++) 
    {
        for (int j = 0; j < mat1->cols; j++) 
        {
            result->data[j][i] = mat1->data[i][j]; //switch the rows and columns here
        }
    }
    return EXIT_SUCCESS;
}

int invert(matrix *A, matrix *result) //we can assume the matrix can be inverted
{
    if(A == NULL || result == NULL || A->rows != A->cols || result->rows != A->rows || result->cols != A->cols)
    {
        return EXIT_SUCCESS; //failure!
    }
    //make the [A | I] matrix
    //gauss-jordan elimination
    //extract the [I] part, and that is our inverse

    //only square matricies are invertible, so we can assume we have an nxn matrix
    int n = A->rows;
    //we are setting up for the augMatrix matrix of [A|I]
    matrix *augMatrix = newMatrix(n, n * 2);

    for (int r = 0; r < n; r++) 
    {
        for (int c = 0; c < n; c++) 
        {
            augMatrix->data[r][c] = A->data[r][c]; //set [A]

        }
    }

    //setting up the identity matrix in the augmented part
    for (int row = 0; row < n; row++) 
    {
        for (int col = 0; col < n; col++) 
        {
                if(row == col) 
            {
                augMatrix->data[row][col+n] = 1.0;
            }
            else
            {
                augMatrix->data[row][col+n] = 0.0;
            }
        
        }
    }

  // Gauss-Jordan elimination wow i love linear algebra
  //go from uppder row to lower row, eliminate everything below the pivot, left to right
  //go from lower row to upper row, eliminate everything above the pivot, right to left
    for (int pivotRow = 0; pivotRow < n; pivotRow++) 
    {
        //make the pivot equal to one first
        double pivotValue = augMatrix->data[pivotRow][pivotRow];
        if (pivotValue != 1.0) 
        {
            for (int col = 0; col < n * 2; col++) // Divide each entry in the pivot row by the pivot value
            {
                augMatrix->data[pivotRow][col] /= pivotValue;
            }
        }

        // make all elements below the pivot equal to 0
        for (int belowPRow = pivotRow + 1; belowPRow < n; belowPRow++) 
        { 
            double factor = augMatrix->data[belowPRow][pivotRow];
            for (int col = 0; col < n * 2; col++) // Update each column in the row
            {
                augMatrix->data[belowPRow][col] -= factor * augMatrix->data[pivotRow][col];
            }
        }

        // make all elements above the pivot equal to 0
        for (int abovePRow = pivotRow - 1; abovePRow >= 0; abovePRow--) 
        { 
            double factor = augMatrix->data[abovePRow][pivotRow];
            for (int col = (n * 2) - 1; col >= 0; col--) // Start from right to left
            { 
                augMatrix->data[abovePRow][col] -= factor * augMatrix->data[pivotRow][col];
            }
        }
}

//we now have the inverse in the [I] section of the [A | I] matrix, so we want to get the inverse
    for (int row = 0; row < n; row++) 
    {
        for (int col = 0; col < n; col++) 
        {
            result->data[row][col] = augMatrix->data[row][col+n]; //copy it to the result matrix
        }
    }

    freeMatrix(augMatrix); //free the memory of the temporary matrix
    return EXIT_SUCCESS; //success!
}


int readTrain(const char *filename, matrix **XMat, matrix **YMat, int *houses, int *attributes) 
{
    //open file, read file
    //check if string is equal to train
    //scan in attributes and sample size
    //create the design matrix X
    //create the output matrix Y

    FILE *file = fopen(filename, "r"); //read file
    if (!file) //if we can't open the file, exit failure 
    {
        return EXIT_FAILURE;
    }

    char *string = (char *)malloc(10 * sizeof(char)); //can't allocate, exit failure
    if (!string) 
    {
        fclose(file);
        return EXIT_FAILURE;
    }

    if (fscanf(file, "%s", string)!= 1 || !(strcmp(string, "train")==0)) //if the string is not scanned in or not = train, failure
    {
        free(string);  
        fclose(file);
        return EXIT_FAILURE;
    }

    int k; //attributes
    int n; //sample number

    if (fscanf(file, "%d %d", &k, &n)!= 2) 
    {
        free(string);
        fclose(file);
        return EXIT_FAILURE;
    }

    *houses = n;
    *attributes = k;
    matrix *X = newMatrix(n, k+1); //we have one extra column just for the 1's
    matrix *Y = newMatrix(n, 1); //one column, n rows for the y 

    // Read data
    for (int i = 0; i < n; i++) 
    {
        X->data[i][0] = 1.0; //initialize the first column to all 1's
        for (int j = 1; j <= k; j++) //for all the other columns
        {
            if (fscanf(file, "%lf", &X->data[i][j]) != 1) //if scanf had a failure of some sort
            {
                freeMatrix(X);
                freeMatrix(Y);
                free(string);
                fclose(file);
                return EXIT_FAILURE;
            }
        }
        if (fscanf(file, "%lf", &Y->data[i][0]) != 1) //scan in last value
        {
            freeMatrix(X);
            freeMatrix(Y);
            free(string);
            fclose(file);
            return EXIT_FAILURE;
        }
    }

    fclose(file);
    free(string);
    *XMat = X;
    *YMat = Y;
    return 0;
}

// Function to read input data for prediction
int readData(const char *filename, matrix **XMat, int k, int *numHouses) 
{
    FILE *file = fopen(filename, "r"); //open file, read access
    if (!file) {
        return EXIT_FAILURE;
    }

    char *string = (char *)malloc(10 * sizeof(char)); //can't allocate, exit failure
    if (!string) 
    {
        fclose(file);
        return EXIT_FAILURE;
    }

    if (fscanf(file, "%s", string) != 1 || !(strcmp(string, "data") == 0)) //if the string is not scanned in or not = data, failure
    {
        free(string);  
        fclose(file);
        return EXIT_FAILURE;
    }

    int k_input;
    int samples;

    if (fscanf(file, "%d %d", &k_input, &samples) != 2) 
    {
        free(string);
        fclose(file);
        return EXIT_FAILURE;
    }

    if (k_input != k) //check to see if K = K for both training and input
    {
        free(string);
        fclose(file);
        return EXIT_FAILURE;
    }

    matrix *xNew = newMatrix(samples, k+1); //creating a new model matrix X with 1 for the first column

    // Read data
    for (int i = 0; i < samples; i++) 
    {
        for (int j = 1; j <= k; j++) 
        {
            xNew->data[i][0] = 1.0; //initilize the first column to zero
            if (fscanf(file, "%lf", &xNew->data[i][j]) != 1) 
            {
                free(string);
                freeMatrix(xNew);
                fclose(file);
                return EXIT_FAILURE;
            }
        }
    }

    //free allocated string
    free(string);
    fclose(file);

    //set pointers
    *XMat = xNew;
    *numHouses = samples;
    return EXIT_SUCCESS;
}


int multiple_linear_regression(const char *train_file, const char *input_file) //putting all the things in here
{
    matrix *X; //x original matrix from trianing
    matrix *Y; //y original matrix from training
    int n = 0; //sample of houses
    int k = 0; //number of attributes

    //read in the training file, get the model X matrix, y matrix, given sample size and attributes
    if(readTrain(train_file, &X, &Y, &n, &k) == 1)
    {
        return EXIT_FAILURE;
    }

    // calculate  equation: W = (X^T X)^-1 X^T Y
    //first tranpose
    //multiply by X
    //invert matrix
    //multiply by X^T
    //multiply by Y
    //get W
    //multiply the data X by W


    matrix *XT = newMatrix(k+1, n); //accomodate one for the column of ones, transpose will have k+1 rows since X has k+1 cols
    int t = transpose(X, XT); //transpose given X, placein XT
    if(t == 1) //failed to transpose
    {
        return EXIT_FAILURE;
    }

    matrix *XTX = newMatrix(k+1, k+1); //X^T has k+1 rows, transpose has k+1 colums, resulting in (k+1)x(k+1) matrix
    int mult = multiply(XT, X, XTX); 
    if(mult == 1)
    {
        return EXIT_FAILURE;
    }

    matrix *XTX_inv = newMatrix(k+1, k+1); //all invertible matricies are square, and we ASSUME XTX is invertible
    int check = invert(XTX, XTX_inv);

    if (check == 1) //failed to invert
    {
        freeMatrix(X);
        freeMatrix(Y);
        freeMatrix(XT);
        freeMatrix(XTX);
        freeMatrix(XTX_inv);
        return EXIT_FAILURE;    
    }


    matrix *XTX_invXT = newMatrix(k+1, n); //multiply inv by XT, with k+1 rows from inv, and n cols for XT
    mult = multiply(XTX_inv, XT, XTX_invXT); //mult
    if(mult == 1)
    {
        return EXIT_FAILURE;
    }

    matrix *W = newMatrix(k+1, 1); //calculate weights
    mult = multiply(XTX_invXT, Y, W); //multiply by Y
    if(mult == 1)
    {
        return EXIT_FAILURE;
    }

    //NEW SECTION
    matrix *xNew;
    int m = 0;
    readData(input_file, &xNew, k, &m); //load in input file, give x a value to point to

    //predict the output by multiplying the X by the W
    matrix *estimate = newMatrix(m, 1);
    mult = multiply(xNew, W, estimate);
    if(mult == 1)
    {
        return EXIT_FAILURE;
    }

    //print the estimate    
    for (int i = 0; i < m; i++) 
    {
        printf("%.0f\n", estimate->data[i][0]);
    }
    //free
    freeMatrix(X);
    freeMatrix(Y);
    freeMatrix(XT);
    freeMatrix(XTX);
    freeMatrix(XTX_inv);
    freeMatrix(XTX_invXT);
    freeMatrix(W);
    freeMatrix(xNew);
    freeMatrix(estimate);

    return EXIT_SUCCESS;
}

int main(int argc, char *argv[]) {
    if (argc != 3) 
    {
        printf("Invalid arguments.");
        return EXIT_FAILURE;
    }

    if(multiple_linear_regression(argv[1], argv[2]) == 0)
    {
        return EXIT_SUCCESS;
    }
    else
    {
        return EXIT_FAILURE;
    }
    
}
