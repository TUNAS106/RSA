#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <stdexcept>

using namespace std;

class BigInt {
private:
    vector<unsigned int> digits; // Base 2^32, little endian
    bool negative;
    
    // xử lý các số 0 thừa
    void removeLeadingZeros() {
        while (digits.size() > 1 && digits.back() == 0) {
            digits.pop_back();
        }
        if (digits.size() == 1 && digits[0] == 0) {
            negative = false;
        }
    }
    
public:

// Constructors
    BigInt() : negative(false) {
        digits.push_back(0);
    }
    
    BigInt(long long n) : negative(false) {
        if (n < 0) {
            negative = true;
            n = -n;
        }
        if (n == 0) {
            digits.push_back(0);
        } else {
            while (n > 0) {
                digits.push_back(n & 0xFFFFFFFF);
                n >>= 32;
            }
        }
    }
    
    static BigInt fromHexLE(const string& hex) {
        BigInt result;
        result.digits.clear();
        
        if (hex.empty() || hex == "0") {
            result.digits.push_back(0);
            return result;
        }
        
        string hexUpper = hex;
        for (char& c : hexUpper) {
            if (c >= 'a' && c <= 'f') c = c - 'a' + 'A';
        }
        
        // Convert little endian to big endian for processing
        string hexBE;
        for (int i = hexUpper.length() - 1; i >= 0; i--) {
            hexBE += hexUpper[i];
        }
        //cout << "debug" << endl;
        //cout << hexUpper << endl;
        //cout << hexBE << endl;
        
        // Read 8 hex chars at a time (32 bits)
        for (int i = hexBE.length(); i > 0; i -= 8) {
            int start = max(0, i - 8);
            string chunk = hexBE.substr(start, i - start);
            
            unsigned long long val = 0;
            for (char c : chunk) {
                val = val * 16;
                if (c >= '0' && c <= '9') val += c - '0';
                else if (c >= 'A' && c <= 'F') val += c - 'A' + 10;
            }
            result.digits.push_back(val);
        }
        
        result.removeLeadingZeros();
        return result;
    }
    
    string toHexLE() const {
        if (negative && !isZero()) return "-1";
        if (isZero()) return "0";
        
        string hexBE;
        for (int i = digits.size() - 1; i >= 0; i--) {
            unsigned int d = digits[i];
            for (int j = 7; j >= 0; j--) {
                int nibble = (d >> (j * 4)) & 0xF;
                hexBE += (nibble < 10) ? ('0' + nibble) : ('A' + nibble - 10);
            }
        }
        
        // Remove leading zeros
        size_t pos = hexBE.find_first_not_of('0');
        if (pos != string::npos) {
            hexBE = hexBE.substr(pos);
        } else {
            hexBE = "0";
        }
        
        // Convert to little endian
        string hexLE;
        for (int i = hexBE.length() - 1; i >= 0; i--) {
            hexLE += hexBE[i];
        }
        
        return hexLE;
    }
    
    bool isZero() const {
        return digits.size() == 1 && digits[0] == 0;
    }
    
    bool isOne() const {
        return !negative && digits.size() == 1 && digits[0] == 1;
    }
    
    bool isNegative() const {
        return negative && !isZero();
    }
    
    // Compare absolute values
    int compareAbs(const BigInt& other) const {
        if (digits.size() != other.digits.size()) {
            return digits.size() < other.digits.size() ? -1 : 1;
        }
        for (int i = digits.size() - 1; i >= 0; i--) {
            if (digits[i] != other.digits[i]) {
                return digits[i] < other.digits[i] ? -1 : 1;
            }
        }
        return 0;
    }
    
    int compare(const BigInt& other) const {
        if (negative != other.negative) {
            return negative ? -1 : 1;
        }
        int cmp = compareAbs(other);
        return negative ? -cmp : cmp;
    }
    
    bool operator<(const BigInt& other) const { return compare(other) < 0; }
    bool operator<=(const BigInt& other) const { return compare(other) <= 0; }
    bool operator>(const BigInt& other) const { return compare(other) > 0; }
    bool operator>=(const BigInt& other) const { return compare(other) >= 0; }
    bool operator==(const BigInt& other) const { return compare(other) == 0; }
    bool operator!=(const BigInt& other) const { return compare(other) != 0; }
    
    // đảo dấu ( ko phải phép trừ)
    BigInt operator-() const {
        BigInt result = *this;
        if (!isZero()) result.negative = !result.negative;
        return result;
    }
    
    // Add absolute values
    BigInt addAbs(const BigInt& other) const {
        BigInt result;
        result.digits.clear();
        
        unsigned long long carry = 0;
        size_t maxSize = max(digits.size(), other.digits.size());
        
        for (size_t i = 0; i < maxSize || carry; i++) {
            unsigned long long sum = carry;
            if (i < digits.size()) sum += digits[i];
            if (i < other.digits.size()) sum += other.digits[i];
            
            result.digits.push_back(sum & 0xFFFFFFFF); // giữ lại 32 bit thấp
            carry = sum >> 32; // phần dư chuyển sang lần kế tiếp
        }
        
        result.removeLeadingZeros();
        return result;
    }
    
    // Subtract absolute values (this must be >= other)
    BigInt subAbs(const BigInt& other) const {
        BigInt result;
        result.digits.clear();
        
        long long borrow = 0;
        for (size_t i = 0; i < digits.size(); i++) {
            long long diff = (long long)digits[i] - borrow;
            if (i < other.digits.size()) {
                diff -= other.digits[i];
            }
            
            if (diff < 0) {
                diff += 0x100000000LL;
                borrow = 1;
            } else {
                borrow = 0;
            }
            
            result.digits.push_back(diff);
        }
        
        result.removeLeadingZeros();
        return result;
    }
    
    BigInt operator+(const BigInt& other) const {
        if (negative == other.negative) {
            BigInt result = addAbs(other);
            result.negative = negative;
            return result;
        } else {
            if (compareAbs(other) >= 0) {
                BigInt result = subAbs(other);
                result.negative = negative;
                return result;
            } else {
                BigInt result = other.subAbs(*this);
                result.negative = other.negative;
                return result;
            }
        }
    }
    
    BigInt operator-(const BigInt& other) const {
        return *this + (-other);
    }
    
    BigInt operator*(const BigInt& other) const {
        BigInt result;
        result.digits.assign(digits.size() + other.digits.size(), 0);
        
        for (size_t i = 0; i < digits.size(); i++) {
            unsigned long long carry = 0;
            for (size_t j = 0; j < other.digits.size() || carry; j++) {
                unsigned long long prod = result.digits[i + j] + carry;
                if (j < other.digits.size()) {
                    prod += (unsigned long long)digits[i] * other.digits[j];
                }
                
                result.digits[i + j] = prod & 0xFFFFFFFF;
                carry = prod >> 32;
            }
        }
        
        result.removeLeadingZeros();
        result.negative = (negative != other.negative) && !result.isZero();
        return result;
    }
    
    // Division for positive numbers only
    pair<BigInt, BigInt> divModPositive(const BigInt& divisor) const {
        if (divisor.isZero()) {
            throw runtime_error("Division by zero");
        }
        
        if (compareAbs(divisor) < 0) {
            return {BigInt(0), *this};
        }
        
        BigInt quotient(0);
        BigInt remainder(0);
        
        for (int i = digits.size() - 1; i >= 0; i--) {
            for (int j = 31; j >= 0; j--) {
                remainder = remainder.addAbs(remainder);
                
                if ((digits[i] >> j) & 1) {
                    remainder = remainder.addAbs(BigInt(1));
                }
                
                if (remainder.compareAbs(divisor) >= 0) {
                    remainder = remainder.subAbs(divisor);
                    if (quotient.digits.size() <= (size_t)i) {
                        quotient.digits.resize(i + 1, 0);
                    }
                    quotient.digits[i] |= (1U << j);
                }
            }
        }
        
        quotient.removeLeadingZeros();
        return {quotient, remainder};
    }
    
    BigInt operator/(const BigInt& other) const {
        bool resNeg = (negative != other.negative);
        BigInt a = *this;
        BigInt b = other;
        a.negative = false;
        b.negative = false;
        
        BigInt q = a.divModPositive(b).first;
        if (resNeg && !q.isZero()) q.negative = true;
        return q;
    }
    
    BigInt operator%(const BigInt& other) const {
        BigInt a = *this;
        BigInt b = other;
        bool aNeg = a.negative;
        a.negative = false;
        b.negative = false;
        
        BigInt r = a.divModPositive(b).second;
        
        // Mathematical modulo: always non-negative
        if (aNeg && !r.isZero()) {
            r = b.subAbs(r);
        }
        
        return r;
    }
};

// Extended Euclidean Algorithm
// Returns gcd, and coefficients x, y such that gcd(a,b) = a*x + b*y
void extendedGCD(const BigInt& a, const BigInt& b, BigInt& gcd, BigInt& x, BigInt& y) {
    if (b.isZero()) {
        gcd = a;
        x = BigInt(1);
        y = BigInt(0);
        return;
    }
    
    BigInt oldR = a, r = b;
    BigInt oldS = BigInt(1), s = BigInt(0);
    BigInt oldT = BigInt(0), t = BigInt(1);
    
    while (!r.isZero()) {
        BigInt q = oldR / r;
        
        BigInt temp = r;
        r = oldR - q * r;
        oldR = temp;
        
        temp = s;
        s = oldS - q * s;
        oldS = temp;
        
        temp = t;
        t = oldT - q * t;
        oldT = temp;
    }
    
    gcd = oldR;
    x = oldS;
    y = oldT;
}

// Compute modular inverse
BigInt modInverse(const BigInt& e, const BigInt& phi) {
    BigInt gcd, x, y;
    extendedGCD(e, phi, gcd, x, y);
    
    if (!gcd.isOne()) {
        return BigInt(-1);
    }
    
    // Make x positive: x = x mod phi
    BigInt result = x % phi;
    
    // Ensure result is positive
    if (result.isNegative() || result.isZero()) {
        result = result + phi;
    }
    
    return result;
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        cerr << "Usage: " << argv[0] << " <input_file> <output_file>" << endl;
        return 1;
    }
    
    ifstream inFile(argv[1]);
    if (!inFile) {
        cerr << "Cannot open input file" << endl;
        return 1;
    }
    
    string pHex, qHex, eHex;
    getline(inFile, pHex);
    getline(inFile, qHex);
    getline(inFile, eHex);
    inFile.close();
    
    BigInt p = BigInt::fromHexLE(pHex);
    BigInt q = BigInt::fromHexLE(qHex);
    BigInt e = BigInt::fromHexLE(eHex);
    
    // phi(N) = (p-1)(q-1)
    BigInt phi = (p - BigInt(1)) * (q - BigInt(1));
    
    // d = e^(-1) mod phi
    BigInt d = modInverse(e, phi);
    
    ofstream outFile(argv[2]);
    if (!outFile) {
        cerr << "Cannot open output file" << endl;
        return 1;
    }
    
    outFile << d.toHexLE() << endl;
    outFile.close();
    
    return 0;
}