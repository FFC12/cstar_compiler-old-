#ifndef BASE_HPP
#define BASE_HPP
#include <fmt/core.h>
#include <fmt/color.h>

enum MessageType {
    INFO,
    WARN,
    ERROR,
    HINT
};

enum MessageStyle {
    NORMAL,
    BOLD,
    ITALIC
};

static void Message(const char* mesg,MessageType type = MessageType::INFO, MessageStyle style = MessageStyle::NORMAL) {
    fmt::print(mesg);
}

static void ErrorMessage(const char* mesg){
    fmt::print(fg(fmt::color::crimson) | fmt::emphasis::bold, mesg);
}

#endif
