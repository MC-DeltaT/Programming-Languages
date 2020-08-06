#include <algorithm>
#include <cstddef>
#include <iostream>
#include <random>
#include <string>
#include <utility>
#include <vector>


class Book {
public:
    Book() : bookID(0), bookName(), ISBN() {}
    Book(int id, std::string name, std::string isbn) : bookID(id), bookName(name), ISBN(isbn) {}
    ~Book() = default;

    int GetBookID() const { return bookID; }
    std::string GetBookName() const { return bookName; }
    std::string GetISBN() const { return ISBN; }

    void SetBookID(int value) { bookID = value; }
    void SetBookName(std::string value) { bookName = value; }
    void SetBookISNB(std::string value) { ISBN = value; }

private:
    int bookID;
    std::string bookName;
    std::string ISBN;
};


bool operator<(Book const& lhs, Book const& rhs) {
    // Assume we're sorting by book name.
    return lhs.GetBookName() < rhs.GetBookName();
}


namespace impl {
    std::size_t partitionBooks(std::vector<Book>& books, std::ptrdiff_t left, std::ptrdiff_t right, std::ptrdiff_t pivot) {
        Book const pivotVal = books.at(pivot);
        std::swap(books.at(pivot), books.at(right));
        std::ptrdiff_t cur = left;
        for (std::ptrdiff_t i = left; i < right; ++i) {
            if (books.at(i) < pivotVal) {
                std::swap(books.at(i), books.at(cur));
                ++cur;
            }
        }
        books.at(right) = books.at(cur);
        books.at(cur) = pivotVal;
        return cur;
    }

    std::ptrdiff_t selectPivot(std::vector<Book>& books, std::ptrdiff_t left, std::ptrdiff_t right) {
        static std::default_random_engine randEng{std::random_device{}()};
        return (randEng() % (right + 1 - left)) + left;
    }

    void sortBooks(std::vector<Book>& books, std::ptrdiff_t left, std::ptrdiff_t right) {
        if (left < right) {
            std::ptrdiff_t const pivot = selectPivot(books, left, right);
            std::ptrdiff_t const newPivot = partitionBooks(books, left, right, pivot);
            sortBooks(books, left, newPivot - 1);
            sortBooks(books, newPivot + 1, right);
        }
    }
}


// Sorts `books` in place using quicksort.
void sortBooks(std::vector<Book>& books) {
    // Using std::ptrdiff_t for things because it's a bit easier than using unsigned.
    // Just assume we won't have > PTRDIFF_MAX elements in the vector.
    impl::sortBooks(books, 0, static_cast<std::ptrdiff_t>(books.size()) - 1);
}



int main()
{
    // Generate some books with random attributes.
    constexpr std::size_t NUM_BOOKS = 100;
    std::vector<Book> books;
    books.reserve(NUM_BOOKS);

    std::string const alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    std::string const digits = "0123456789";
    std::default_random_engine randEng{std::random_device{}()};
    for (std::size_t i = 0; i < NUM_BOOKS; ++i) {
        int const id = randEng() % 1000000;

        std::size_t const nameLength = randEng() % 7 + 3;
        std::string name(nameLength, '\0');
        std::generate(name.begin(), name.end(), [&]() {
            return alphabet[randEng() % alphabet.size()];
        });

        std::string isbn(13, '\0');
        std::generate(isbn.begin(), isbn.end(), [&]() {
            return digits[randEng() % digits.size()];
        });

        books.emplace_back(id, name, isbn);
    }

    sortBooks(books);

    for (Book const& book : books) {
        std::cout << book.GetBookName() << std::endl;
    }
}
