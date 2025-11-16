#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <stdexcept>
#include <random> 
#include <climits>

using namespace std;

class BigInt {
private:
    vector<unsigned int> digits; // Base 2^32, little endian
    bool negative;
    
    void removeLeadingZeros() {
        while (digits.size() > 1 && digits.back() == 0) {
            digits.pop_back();
        }
        if (digits.size() == 1 && digits[0] == 0) {
            negative = false;
        }
    }
    
public:
    BigInt() : negative(false) {
        digits.push_back(0);
    }
    unsigned int getLeastSignificantDigit() const {
        return digits[0];
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
            
            result.digits.push_back(sum & 0xFFFFFFFF);
            carry = sum >> 32;
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
    friend BigInt randomBigInt(const BigInt& max, mt19937_64& gen);
    friend BigInt binaryPower(BigInt a, BigInt k, const BigInt& n);
    friend bool millerRabinTest(const BigInt& a, const BigInt& n, long long k, const BigInt& m);
    friend bool isPrimeMillerRabin(const BigInt& n);
};


BigInt binaryPower(BigInt a, BigInt k, const BigInt& n) {
    a = a % n;
    BigInt res(1);
    while (!k.isZero()) {
        if (k.digits[0] & 1) {
            res = (res * a) % n;
        }
        a = (a * a) % n;
        k = k / BigInt(2);
    }
    return res;
}
BigInt randomBigInt(const BigInt& max, mt19937_64& gen) {
    if (max <= BigInt(2)) return BigInt(2);
    
    BigInt result;
    result.digits.clear();
    
    
    size_t numDigits = max.digits.size();
    
    uniform_int_distribution<unsigned int> dis(0, UINT_MAX);
    
    for (size_t i = 0; i < numDigits; i++) {
        result.digits.push_back(dis(gen));
    }
    
    result.removeLeadingZeros();
    
    if (result.isZero() || result.isOne()) {
        result = BigInt(2);
    } else if (result >= max) {
        result = result % max;
        if (result < BigInt(2)) {
            result = BigInt(2);
        }
    }
    
    return result;
}

bool millerRabinTest(const BigInt& a, const BigInt& n, long long k, const BigInt& m) {
    BigInt mod = binaryPower(a, m, n);
    if (mod.isOne() || mod == (n - BigInt(1)))
        return true;
        
    for (int l = 1; l < k; ++l) {
        mod = (mod * mod) % n;
        if (mod == (n - BigInt(1)))
            return true;
    }
    return false;
}

bool isPrimeMillerRabin(const BigInt& n) {
    if (n == BigInt(2) || n == BigInt(3) || 
        n == BigInt(5) || n == BigInt(7))
        return true;
    if (n < BigInt(11))
        return false;
    
    if ((n.digits[0] & 1) == 0)
        return false;
    
    long long k = 0;
    BigInt m = n - BigInt(1);
    while ((m.digits[0] & 1) == 0) {
        m = m / BigInt(2);
        k++;
    }
    
    const int repeatTime = 20;
    vector<int> smallPrimes = {2, 3, 5, 7, 11, 13, 17, 19, 23};
    
    for (int prime : smallPrimes) {
        BigInt a(prime);
        if (a >= n) continue;
        
        if (!millerRabinTest(a, n, k, m))
            return false;
    }
    
    random_device rd;
    mt19937_64 gen(rd());
    int testsRemaining = repeatTime - min((int)smallPrimes.size(), (int)repeatTime);
    for (int i = 0; i < testsRemaining; ++i) {
        BigInt maxVal = n - BigInt(2);
        if (maxVal <= BigInt(2)) break;
        
        BigInt a = randomBigInt(maxVal, gen);
        
        if (!millerRabinTest(a, n, k, m))
            return false;
    }
    return true;
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
    
    string pHex;
    getline(inFile, pHex);

    inFile.close();
    
    BigInt p = BigInt::fromHexLE(pHex);

    int res = 0;   
    // Kiểm tra tính nguyên tố của p
    if (isPrimeMillerRabin(p)) {
        res = 1;
    }
    
    ofstream outFile(argv[2]);
    if (!outFile) {
        cerr << "Cannot open output file" << endl;
        return 1;
    }
    
    outFile << res << endl;
    outFile.close();
    
    return 0;
}