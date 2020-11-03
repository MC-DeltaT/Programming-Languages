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
    // Using std::ptrdiff_t for things because it's a bit easier and safer than using unsigned.
    // Just assume we won't have > PTRDIFF_MAX elements in the vector.
    impl::sortBooks(books, 0, static_cast<std::ptrdiff_t>(books.size()) - 1);
}


void doTest(std::string testName, std::vector<Book> books) {
    std::cout << testName << std::endl;
    sortBooks(books);
    for (Book const& book : books) {
        std::cout << "  " << book.GetBookName() << std::endl;
    }
    std::cout << std::endl << std::endl;
}


int main() {
    std::vector<Book> books1{
        {1, "BarQux", "98693457"},
        {2, "Effective Modern C++", "235096343"},
        {0, "Foo", "0123456789"},
        {3, "Harry Potter", "576098234"},
        {4, "The Lord of the Rings", "004563896"}
    };
    doTest("In order", books1);

    std::vector<Book> books2{
        {4, "The Lord of the Rings", "004563896"},
        {3, "Harry Potter", "576098234"},
        {0, "Foo", "0123456789"},
        {2, "Effective Modern C++", "235096343"},
        {1, "BarQux", "98693457"}
    };
    doTest("Reverse order", books2);

    std::vector<Book> books3{
        {0, "Foo", "0123456789"},
        {1, "BarQux", "98693457"},
        {2, "Effective Modern C++", "235096343"},
        {3, "Harry Potter", "576098234"},
        {4, "The Lord of the Rings", "004563896"}
    };
    doTest("Random order", books3);

    std::vector<Book> books4;
    doTest("Empty array", books4);
}
