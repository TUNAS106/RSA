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

    void removeLeadingZeros() {
        while (digits.size() > 1 && digits.back() == 0)
            digits.pop_back();
        if (digits.size() == 1 && digits[0] == 0)
            negative = false;
    }

public:
    // Constructors
    BigInt() : negative(false) { digits.push_back(0); }
    BigInt(long long n) : negative(false) {
        if (n < 0) { negative = true; n = -n; }
        if (n == 0) digits.push_back(0);
        else while (n > 0) { digits.push_back(n & 0xFFFFFFFF); n >>= 32; }
    }

    static BigInt fromHexLE(const string& hex) {
        BigInt result; result.digits.clear();
        if (hex.empty() || hex == "0") { result.digits.push_back(0); return result; }

        string hexUpper = hex;
        for (char& c : hexUpper) if (c >= 'a' && c <= 'f') c = c - 'a' + 'A';

        string hexBE;
        for (int i = hexUpper.length() - 1; i >= 0; i--) hexBE += hexUpper[i];

        for (int i = hexBE.length(); i > 0; i -= 8) {
            int start = max(0, i - 8);
            string chunk = hexBE.substr(start, i - start);
            unsigned long long val = 0;
            for (char c : chunk) {
                val = val * 16;
                if (c >= '0' && c <= '9') val += c - '0';
                else val += c - 'A' + 10;
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

        size_t pos = hexBE.find_first_not_of('0');
        if (pos != string::npos) hexBE = hexBE.substr(pos);
        else hexBE = "0";

        string hexLE;
        for (int i = hexBE.length() - 1; i >= 0; i--) hexLE += hexBE[i];
        return hexLE;
    }

    string toBinary() const {
        if (isZero()) return "0";
        
        string binary;
        for (int i = digits.size() - 1; i >= 0; i--) {
            unsigned int d = digits[i];
            for (int j = 31; j >= 0; j--) {
                binary += ((d >> j) & 1) ? '1' : '0';
            }
        }
        size_t pos = binary.find_first_not_of('0');
        if (pos != string::npos)
            return binary.substr(pos);
        return "0";
    }

    static bool bitAtPosition(const string& binaryStr, int position) {
        if (position < 1 || position > (int)binaryStr.length())
            return false;
        return binaryStr[binaryStr.length() - position] == '1';
    }

    bool isZero() const { return digits.size() == 1 && digits[0] == 0; }

    // shift right 1 bit
    void shiftRight1() {
        bool carry = false;
        for (int i = digits.size() - 1; i >= 0; --i) {
            bool nextCarry = digits[i] & 1;
            digits[i] = (digits[i] >> 1) | (carry ? 0x80000000 : 0);
            carry = nextCarry;
        }
        removeLeadingZeros();
    }

    // Compare absolute values
    int compareAbs(const BigInt& other) const {
        if (digits.size() != other.digits.size())
            return digits.size() < other.digits.size() ? -1 : 1;
        for (int i = digits.size() - 1; i >= 0; i--)
            if (digits[i] != other.digits[i])
                return digits[i] < other.digits[i] ? -1 : 1;
        return 0;
    }

    BigInt addAbs(const BigInt& other) const {
        BigInt result; result.digits.clear();
        unsigned long long carry = 0;
        size_t maxSize = max(digits.size(), other.digits.size());
        for (size_t i = 0; i < maxSize || carry; i++) {
            unsigned long long sum = carry;
            if (i < digits.size()) sum += digits[i];
            if (i < other.digits.size()) sum += other.digits[i];
            result.digits.push_back(sum & 0xFFFFFFFF);
            carry = sum >> 32;
        }
        result.removeLeadingZeros(); return result;
    }

    BigInt subAbs(const BigInt& other) const {
        BigInt result; result.digits.clear();
        long long borrow = 0;
        for (size_t i = 0; i < digits.size(); i++) {
            long long diff = (long long)digits[i] - borrow;
            if (i < other.digits.size()) diff -= other.digits[i];
            if (diff < 0) { diff += 0x100000000LL; borrow = 1; }
            else borrow = 0;
            result.digits.push_back(diff);
        }
        result.removeLeadingZeros(); return result;
    }

    BigInt operator-(const BigInt& other) const { return *this + (-other); }
    BigInt operator-() const { BigInt r = *this; r.negative = !r.negative; return r; }
    BigInt operator+(const BigInt& other) const {
        if (negative == other.negative) { BigInt r = addAbs(other); r.negative = negative; return r; }
        if (compareAbs(other) >= 0) { BigInt r = subAbs(other); r.negative = negative; return r; }
        BigInt r = other.subAbs(*this); r.negative = other.negative; return r;
    }

    BigInt operator*(const BigInt& other) const {
        BigInt result; result.digits.assign(digits.size() + other.digits.size(), 0);
        for (size_t i = 0; i < digits.size(); i++) {
            unsigned long long carry = 0;
            for (size_t j = 0; j < other.digits.size() || carry; j++) {
                unsigned long long prod = result.digits[i + j] + carry;
                if (j < other.digits.size()) prod += (unsigned long long)digits[i] * other.digits[j];
                result.digits[i + j] = prod & 0xFFFFFFFF;
                carry = prod >> 32;
            }
        }
        result.removeLeadingZeros();
        result.negative = (negative != other.negative) && !result.isZero();
        return result;
    }

    pair<BigInt, BigInt> divModPositive(const BigInt& divisor) const {
        if (divisor.isZero()) throw runtime_error("Division by zero");
        if (compareAbs(divisor) < 0) return {BigInt(0), *this};
        BigInt quotient(0), remainder(0);
        for (int i = digits.size() - 1; i >= 0; i--) {
            for (int j = 31; j >= 0; j--) {
                remainder = remainder.addAbs(remainder);
                if ((digits[i] >> j) & 1) remainder = remainder.addAbs(BigInt(1));
                if (remainder.compareAbs(divisor) >= 0) {
                    remainder = remainder.subAbs(divisor);
                    if (quotient.digits.size() <= (size_t)i) quotient.digits.resize(i + 1, 0);
                    quotient.digits[i] |= (1U << j);
                }
            }
        }
        quotient.removeLeadingZeros(); return {quotient, remainder};
    }

    BigInt operator/(const BigInt& other) const {
        bool resNeg = (negative != other.negative);
        BigInt a = *this, b = other; a.negative = false; b.negative = false;
        BigInt q = a.divModPositive(b).first;
        if (resNeg && !q.isZero()) q.negative = true;
        return q;
    }

    BigInt operator%(const BigInt& other) const {
        BigInt a = *this, b = other; a.negative = false; b.negative = false;
        BigInt r = a.divModPositive(b).second;
        return r;
    }

    friend BigInt modPow(const BigInt& x, const BigInt& k, const BigInt& n);
};

// --- Binary exponentiation using your algorithm ---
BigInt modPow(const BigInt& x, const BigInt& k, const BigInt& n) {
    if (k.isZero()) return BigInt(1);
    
    BigInt result(1);
    BigInt temp = x % n;
    string Kbin = k.toBinary();
    
    int base = 1;
    while (base <= (int)Kbin.length()) {
        if (BigInt::bitAtPosition(Kbin, base)) {
            result = (result * temp) % n;
        }
        temp = (temp * temp) % n;
        base++;
    }
    
    return (result + n) % n;
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        cerr << "Usage: " << argv[0] << " <input_file> <output_file>" << endl;
        return 1;
    }

    ifstream inFile(argv[1]);
    if (!inFile) { cerr << "Cannot open input file\n"; return 1; }

    string NHex, kHex, xHex;
    getline(inFile, NHex);
    getline(inFile, kHex);
    getline(inFile, xHex);
    inFile.close();

    BigInt N = BigInt::fromHexLE(NHex);
    BigInt k = BigInt::fromHexLE(kHex);
    BigInt x = BigInt::fromHexLE(xHex);

    BigInt y = modPow(x, k, N);

    ofstream outFile(argv[2]);
    if (!outFile) { cerr << "Cannot open output file\n"; return 1; }
    outFile << y.toHexLE() << endl;
    outFile.close();

    return 0;
}