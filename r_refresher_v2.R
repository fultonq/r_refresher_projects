# ==============================================================================
# R PROGRAMMING REFRESHER
# ==============================================================================
# Self-contained, runnable top to bottom (Rscript or source()). No external
# files needed -- Section 17 builds and reads its own temp CSV. Every example
# explicitly print()s/cat()s its result rather than relying on console
# auto-print, since auto-print does NOT happen when a script runs in batch
# mode (Rscript) -- only interactively at the console.
#
# Only non-base dependency: MASS, for rlm() in Section 22. MASS ships bundled
# with every standard R installation ("recommended" package) -- it is not
# the kind of source-compile-heavy install `car` was. The script checks for
# it and skips gracefully if truly absent.
# ==============================================================================

section <- function(num, title) {
  bar <- paste(rep("=", 78), collapse = "")
  cat("\n", bar, "\n", sep = "")
  cat(sprintf("%2s. %s\n", num, title))
  cat(bar, "\n", sep = "")
}

# ------------------------------------------------------------------------------
section(1, "LOGICAL VECTORS")
# ------------------------------------------------------------------------------
temps <- c(58, 72, 91, 65, 80, NA, 45)

is_hot   <- temps > 75          # element-wise comparison -> logical vector
is_valid <- !is.na(temps)       # ! negates; is.na() flags missing values

cat("is_hot:\n");   print(is_hot)
cat("is_valid:\n"); print(is_valid)

# Logical vectors are 0/1 under the hood, so they can be summed directly
cat("Number of hot days (NA-safe):", sum(is_hot, na.rm = TRUE), "\n")
cat("Number of missing readings:  ", sum(!is_valid), "\n")

# & and | are VECTORIZED (compare every element); && and || look only at the
# FIRST element and are meant for single if()-style conditions, not vectors.
cat("Hot AND on-record (element-wise):\n")
print(is_hot & is_valid)

if (temps[1] > 50 && temps[1] < 100) {
  cat("First reading is in a normal range (single-value check uses &&).\n")
}

# ------------------------------------------------------------------------------
section(2, "INDEX VECTORS -- SELECTING AND MODIFYING SUBSETS")
# ------------------------------------------------------------------------------
scores <- c(Amy = 88, Ben = 45, Cara = 91, Drew = 67, Eve = 100)

cat("Position 2:\n");                    print(scores[2])
cat("By name 'Cara':\n");                print(scores["Cara"])
cat("Everything except position 1:\n");  print(scores[-1])
cat("Positions 1 and 3:\n");             print(scores[c(1, 3)])
cat("Logical filter (< 60):\n");         print(scores[scores < 60])

# Index vectors work the same way on the LEFT of <- to modify a subset in
# place -- classic "curve the failing scores" pattern.
scores[scores < 60] <- scores[scores < 60] + 10
cat("After curving failing scores by +10:\n")
print(scores)

# ------------------------------------------------------------------------------
section(3, "OBJECTS, THEIR MODES AND ATTRIBUTES")
# ------------------------------------------------------------------------------
a <- 5L; b <- 5.5; d <- "five"; e <- TRUE; f <- 2 + 3i

for (obj in list(a, b, d, e, f)) {
  cat(sprintf("value=%-8s  typeof=%-10s  mode=%-10s  class=%s\n",
              format(obj), typeof(obj), mode(obj), class(obj)[1]))
}

# Attributes are metadata attached to an object -- dim, names, class, etc.
x <- 1:6
attr(x, "dim") <- c(2, 3)     # equivalent to dim(x) <- c(2, 3)
cat("x after gaining a dim attribute (now prints as a matrix):\n")
print(x)
cat("attributes(x):\n")
print(attributes(x))

# structure() builds an object and its attributes in one call
labeled <- structure(1:3, names = c("lo", "mid", "hi"), note = "demo vector")
cat("labeled:\n"); print(labeled)
cat("attributes(labeled):\n"); print(attributes(labeled))

# ------------------------------------------------------------------------------
section(4, "ARRAYS")
# ------------------------------------------------------------------------------
# An array is just a vector with a `dim` attribute -- same data, N dimensions.
cube <- array(1:24, dim = c(2, 3, 4))   # 2 rows x 3 cols x 4 "layers"
cat("Dimensions of cube:", dim(cube), "\n")
cat("cube[, , 1] -- first layer only:\n")
print(cube[, , 1])

# The simplest array is a plain vector that gains a dim attribute after the fact
v <- 1:12
dim(v) <- c(3, 4)     # v is now a 3x4 matrix -- a 2-D array
cat("v after dim(v) <- c(3, 4):\n")
print(v)

# ------------------------------------------------------------------------------
section(5, "ARRAY INDEXING -- SUBSECTIONS OF AN ARRAY")
# ------------------------------------------------------------------------------
A <- array(1:24, dim = c(2, 3, 4))

cat("A[1, , ]  (row 1 across every column/layer):\n"); print(A[1, , ])
cat("A[, 2, ]  (column 2 across every row/layer):\n"); print(A[, 2, ])
cat("A[1, 2, 3] (a single element):", A[1, 2, 3], "\n")

cat("Without drop=FALSE, a length-1 dim collapses away:", dim(A[, , 1]), "\n")
cat("With drop=FALSE, the 3-D shape is preserved:      ", dim(A[, , 1, drop = FALSE]), "\n")

# ------------------------------------------------------------------------------
section(6, "INDEX MATRICES, INCIDENCE MATRIX")
# ------------------------------------------------------------------------------
# A matrix of subscripts: each ROW supplies one full (row, col) coordinate.
M <- matrix(1:20, nrow = 4)
picks <- matrix(c(1, 1,   2, 3,   4, 5), ncol = 2, byrow = TRUE)  # 3 coordinate pairs
cat("M:\n"); print(M)
cat("Elements at those 3 coordinates (picks):", M[picks], "\n")

# Incidence / contingency matrix: rows = observations, columns = categories,
# each cell = a count of co-occurrence. table() builds this directly.
students  <- c("Amy", "Ben", "Cara", "Amy", "Drew")
clubs     <- factor(c("Chess", "Chess", "Art", "Art", "Chess"))
incidence <- table(students, clubs)
cat("Incidence matrix (student x club):\n")
print(incidence)

# ------------------------------------------------------------------------------
section(7, "THE array() FUNCTION")
# ------------------------------------------------------------------------------
# array(data, dim, dimnames) -- `data` RECYCLES to fill the requested shape.
recycled <- array(1:4, dim = c(2, 4))    # only 4 values fill an 8-slot array
cat("array(1:4, dim=c(2,4)) -- values recycle to fill the shape:\n")
print(recycled)

named_arr <- array(1:6, dim = c(2, 3),
                    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
cat("named_arr with dimnames:\n"); print(named_arr)
cat("named_arr['r2', 'c3'] :", named_arr["r2", "c3"], "\n")

# ------------------------------------------------------------------------------
section(8, "THE OUTER PRODUCT OF TWO ARRAYS")
# ------------------------------------------------------------------------------
x <- 1:4
y <- 1:3

cat("outer(x, y) -- default FUN='*', every x[i]*y[j] combination:\n")
print(outer(x, y))
cat("x %o% y -- infix shorthand, identical result:\n")
print(x %o% y)

# outer() accepts ANY two-argument function, not just multiplication
f <- function(x, y) cos(y) / (1 + x^2)
z <- outer(x, y, f)
cat("outer(x, y, f) with a custom function f(x,y) = cos(y)/(1+x^2):\n")
print(z)

# ------------------------------------------------------------------------------
section(9, "GENERALIZED TRANSPOSE OF AN ARRAY")
# ------------------------------------------------------------------------------
# t() only understands 2-D matrices. aperm() generalizes transpose to any
# number of dimensions by permuting the ORDER of the dimensions.
A3 <- array(1:24, dim = c(2, 3, 4))
cat("Original dims:", dim(A3), "\n")

A3t <- aperm(A3, perm = c(3, 2, 1))   # reverse the dimension order
cat("Permuted dims (perm = c(3,2,1)):", dim(A3t), "\n")

# On a plain 2-D matrix, aperm(A, c(2,1)) IS t(A)
mat <- matrix(1:6, nrow = 2)
cat("t(mat) identical to aperm(mat, c(2,1))?",
    identical(t(mat), aperm(mat, c(2, 1))), "\n")

# ------------------------------------------------------------------------------
section(10, "MATRIX FACILITIES")
# ------------------------------------------------------------------------------
Am <- matrix(1:4, nrow = 2)
Bm <- matrix(5:8, nrow = 2)

cat("Element-wise Am * Bm:\n");         print(Am * Bm)
cat("True matrix product Am %*% Bm:\n"); print(Am %*% Bm)
cat("Transpose t(Am):\n");              print(t(Am))
cat("Diagonal of Am:", diag(Am), "\n")
cat("3x3 identity via diag(3):\n");     print(diag(3))

# ------------------------------------------------------------------------------
section(11, "FORMING PARTITIONED MATRICES: cbind() AND rbind()")
# ------------------------------------------------------------------------------
top    <- matrix(1:6, nrow = 2)
bottom <- matrix(7:9, nrow = 1)
side   <- matrix(10:11, nrow = 2)

cat("rbind(top, bottom) -- blocks stacked vertically (columns must match):\n")
print(rbind(top, bottom))
cat("cbind(top, side) -- blocks stacked horizontally (rows must match):\n")
print(cbind(top, side))
cat("cbind() also builds a matrix straight from plain named vectors:\n")
print(cbind(id = 1:3, score = c(88, 91, 76)))

# ------------------------------------------------------------------------------
section(12, "THE CONCATENATION FUNCTION c() WITH ARRAYS")
# ------------------------------------------------------------------------------
Mc <- matrix(1:6, nrow = 2)
cat("Mc has dim:", dim(Mc), "\n")

flattened <- c(Mc)   # GOTCHA: c() strips the dim attribute entirely
cat("c(Mc) drops the dim attribute -- is.null(dim(flattened)):",
    is.null(dim(flattened)), "\n")
cat("flattened:", flattened, "\n")
cat("(If you want to combine arrays and KEEP a shape, use rbind/cbind/array,\n")
cat(" not c() -- this is a very common source of 'why did my matrix vanish' bugs.)\n")

# ------------------------------------------------------------------------------
section(13, "FREQUENCY TABLES FROM FACTORS")
# ------------------------------------------------------------------------------
grade_level <- factor(c("Freshman", "Senior", "Junior", "Freshman", "Senior", "Senior"),
                       levels = c("Freshman", "Sophomore", "Junior", "Senior"))

cat("Counts per level (note Sophomore correctly shows 0, not omitted):\n")
print(table(grade_level))

pass_fail <- factor(c("Pass", "Fail", "Pass", "Pass", "Fail", "Pass"))
cat("2-D cross-tabulation table(grade_level, pass_fail):\n")
print(table(grade_level, pass_fail))

# cut() turns a continuous variable into a factor of bins -- table() then
# works on it exactly the same way as on any other factor.
raw_scores <- c(55, 72, 91, 63, 88, 45, 77)
brackets <- cut(raw_scores, breaks = c(0, 60, 70, 80, 90, 100),
                 labels = c("F", "D", "C", "B", "A"))
cat("cut() bins a continuous variable into a factor:\n")
print(table(brackets))

# ------------------------------------------------------------------------------
section(14, "LISTS")
# ------------------------------------------------------------------------------
employee <- list(name = "Dana Reyes", id = 4471, active = TRUE,
                  skills = c("R", "Python", "SQL"))

cat("employee[['skills']] -- unwraps to the actual vector:\n")
print(employee[["skills"]])
cat("employee['skills']   -- stays wrapped in a length-1 SUB-LIST:\n")
print(employee["skills"])
cat("employee$name -- shorthand for [[ ]] by exact name:", employee$name, "\n")

cat("class(employee[['skills']]):", class(employee[["skills"]]), "\n")
cat("class(employee['skills']):  ", class(employee["skills"]), "\n")

# ------------------------------------------------------------------------------
section(15, "CONSTRUCTING AND MODIFYING LISTS")
# ------------------------------------------------------------------------------
roster <- list()                          # start empty
roster$Amy       <- list(grade = 88, club = "Chess")
roster$Ben       <- list(grade = 45, club = "Art")
roster[["Cara"]] <- list(grade = 91, club = "Chess")

roster$Ben$grade <- 65    # modify a nested value in place

roster$Ben <- NULL        # NULL assignment DELETES a list element entirely
cat("Names remaining after roster$Ben <- NULL:", names(roster), "\n")

# modifyList() merges updates in without hand-writing every field
roster$Amy <- modifyList(roster$Amy, list(grade = 95, honors = TRUE))
cat("roster$Amy after modifyList():\n")
print(roster$Amy)

# ------------------------------------------------------------------------------
section(16, "DATA FRAMES")
# ------------------------------------------------------------------------------
df <- data.frame(
  Name  = c("Amy", "Ben", "Cara"),
  Grade = c(88, 65, 91),
  Club  = c("Chess", "Art", "Chess"),
  stringsAsFactors = FALSE
)

cat("str(df):\n"); str(df)
cat("Column access df$Grade:", df$Grade, "\n")
cat("Row filter -- same logical-index pattern as Section 2:\n")
print(df[df$Grade >= 90, ])
cat("Column selection by name:\n")
print(df[, c("Name", "Club")])

# ------------------------------------------------------------------------------
section(17, "READING DATA FROM FILES")
# ------------------------------------------------------------------------------
# read.csv()/write.csv() are the everyday workhorses. Demonstrated with a temp
# file so this example runs anywhere with no external data dependency.
tmp <- tempfile(fileext = ".csv")
write.csv(df, tmp, row.names = FALSE)

reloaded <- read.csv(tmp)
cat("Reloaded from CSV:\n"); print(reloaded)
cat("str(reloaded) -- Club comes back as character, not factor:\n")
str(reloaded)   # stringsAsFactors has defaulted to FALSE in read.csv since R 4.0

# read.table() with text= is handy for quick inline data with no file at all
inline <- read.table(text = "id value
1 10.5
2 22.1
3 7.8", header = TRUE)
cat("Inline text read via read.table(text = ...):\n")
print(inline)

unlink(tmp)   # clean up the temp file

# ------------------------------------------------------------------------------
section(18, "GROUPING, LOOPS AND CONDITIONAL EXECUTION")
# ------------------------------------------------------------------------------
for (i in 1:5) {
  if (i %% 2 == 0) next            # skip even numbers
  cat("for-loop odd value:", i, "\n")
}

i <- 0
while (i < 3) {
  cat("while iteration i =", i, "\n")
  i <- i + 1
}

count <- 0
repeat {
  count <- count + 1
  if (count >= 2) break
}
cat("repeat ran until count =", count, "\n")

# ifelse() is VECTORIZED -- use it for whole-column logic. Plain if/else only
# evaluates a single value and errors on a length > 1 condition in modern R.
grade_letter <- ifelse(df$Grade >= 90, "A", ifelse(df$Grade >= 70, "B", "F"))
cat("Vectorized grade_letter:", grade_letter, "\n")

# switch() for clean multi-branch dispatch on one value
describe <- function(g) switch(g, A = "Excellent", B = "Good", "Needs work")
cat("describe('A') =", describe("A"), " | describe('Z') =", describe("Z"), "\n")

# ------------------------------------------------------------------------------
section(19, "solve()")
# ------------------------------------------------------------------------------
As <- matrix(c(2, 1, 1, 3), nrow = 2)
bs <- c(5, 6)

xs <- solve(As, bs)     # solves As %*% x = bs -- preferred over inverting by hand
cat("Solution x to As %*% x = bs:", xs, "\n")
cat("Check As %*% x - bs (should be ~0):", As %*% xs - bs, "\n")

As_inv <- solve(As)    # solve() with no `b` returns the matrix inverse
cat("Inverse of As:\n"); print(As_inv)
cat("As %*% As_inv (should be the identity matrix):\n"); print(As %*% As_inv)

# ------------------------------------------------------------------------------
section(20, "EIGENVALUES AND EIGENVECTORS")
# ------------------------------------------------------------------------------
S <- matrix(c(4, 2, 2, 3), nrow = 2)   # symmetric -> real eigenvalues guaranteed
ev <- eigen(S)

cat("Eigenvalues:\n"); print(ev$values)
cat("Eigenvectors (columns):\n"); print(ev$vectors)

# Definition check: S %*% v should equal lambda * v for each eigenpair
v1 <- ev$vectors[, 1]
lambda1 <- ev$values[1]
cat("S %*% v1:    ", S %*% v1, "\n")
cat("lambda1 * v1:", lambda1 * v1, "\n")

# ------------------------------------------------------------------------------
section(21, "SINGULAR VALUE DECOMPOSITION AND DETERMINANTS")
# ------------------------------------------------------------------------------
Msvd <- matrix(c(3, 1, 1, 3, 2, 0), nrow = 2)  # 2x3 -- SVD doesn't require square

s <- svd(Msvd)
cat("Singular values:", s$d, "\n")

reconstructed <- s$u %*% diag(s$d) %*% t(s$v)
cat("Max |original - reconstructed| (should be ~0):",
    max(abs(Msvd - reconstructed)), "\n")

sq <- matrix(c(2, 1, 1, 3), nrow = 2)
cat("det(sq) =", det(sq), "\n")
cat("A matrix is invertible only when det != 0 -- this is exactly why solve()\n")
cat("fails on a singular matrix.\n")

# ------------------------------------------------------------------------------
section(22, "REGRESSION TOOLKIT: lm, glm, rlm, qqnorm, lines, abline, attach/detach")
# ------------------------------------------------------------------------------
set.seed(1)
n <- 40
study_hours <- runif(n, 1, 10)
exam_score  <- 50 + 4.5 * study_hours + rnorm(n, sd = 8)
exam_df     <- data.frame(study_hours, exam_score)

# ---- lm(): ordinary least squares -------------------------------------------
fit <- lm(exam_score ~ study_hours, data = exam_df)
cat("summary(lm(exam_score ~ study_hours)):\n")
print(summary(fit))

plot(exam_df$study_hours, exam_df$exam_score,
     xlab = "Study Hours", ylab = "Exam Score", main = "OLS Fit with Diagnostics")
abline(fit, col = "steelblue", lwd = 2)                        # abline() understands an lm object directly
abline(h = mean(exam_df$exam_score), col = "gray", lty = 2)    # or plain h=/v=/a=,b= reference lines
lines(lowess(exam_df$study_hours, exam_df$exam_score),         # lines() adds any x/y path to an existing plot
      col = "darkorange", lwd = 2)

# ---- qqnorm(): are the residuals plausibly normal? ---------------------------
qqnorm(residuals(fit), main = "Residual Normal Q-Q Plot")
qqline(residuals(fit), col = "red")   # reference line for a perfect normal fit

# ---- glm(): generalized linear model, e.g. logistic regression ---------------
exam_df$passed <- as.integer(exam_df$exam_score >= 70)
logit_fit <- glm(passed ~ study_hours, data = exam_df, family = binomial)
cat("summary(glm(passed ~ study_hours, family = binomial)):\n")
print(summary(logit_fit))
cat("Predicted P(pass) at 8 study hours:",
    predict(logit_fit, newdata = data.frame(study_hours = 8), type = "response"), "\n")

# ---- rlm(): robust regression, resistant to outliers (MASS package) ----------
if (requireNamespace("MASS", quietly = TRUE)) {
  library(MASS)

  exam_outliers <- exam_df
  exam_outliers$exam_score[1:3] <- c(5, 3, 8)   # inject 3 severe outliers

  fit_ols <- lm(exam_score ~ study_hours, data = exam_outliers)
  fit_rob <- rlm(exam_score ~ study_hours, data = exam_outliers)

  cat("OLS slope (dragged toward the outliers):    ", coef(fit_ols)["study_hours"], "\n")
  cat("Robust rlm slope (resists the outliers):     ", coef(fit_rob)["study_hours"], "\n")

  plot(exam_outliers$study_hours, exam_outliers$exam_score,
       xlab = "Study Hours", ylab = "Exam Score",
       main = "OLS vs Robust Regression in the Presence of Outliers")
  abline(fit_ols, col = "red", lwd = 2)
  abline(fit_rob, col = "darkgreen", lwd = 2)
  legend("topleft", legend = c("lm (OLS)", "rlm (robust)"),
         col = c("red", "darkgreen"), lty = 1, lwd = 2, bty = "n")
} else {
  cat("MASS not found -- skipping rlm() demo. MASS ships bundled with base R by\n")
  cat("default; if it's genuinely missing, install.packages('MASS') is a small,\n")
  cat("fast install (unlike car's heavier compiled dependency chain).\n")
}

# ---- attach() / detach(): classic material, but use sparingly ----------------
# attach() puts a data frame's columns onto the search path so you can write
# `study_hours` instead of `exam_df$study_hours`. It's standard intro-R
# material, but modern style avoids habitual use: it silently masks any other
# object of the same name and is a common source of "which variable am I
# actually using" bugs, especially once you attach more than one data frame.
attach(exam_df)
cat("Mean study_hours via attach():", mean(study_hours), "\n")
detach(exam_df)     # always detach what you attach

# Safer modern equivalent -- scoped to one expression, leaves no lingering
# entries on the search path:
cat("Mean study_hours via with():  ", with(exam_df, mean(study_hours)), "\n")

cat("\n", paste(rep("=", 78), collapse = ""), "\n", sep = "")
cat("REFRESHER COMPLETE.\n")
