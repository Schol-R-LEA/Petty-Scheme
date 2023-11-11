#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

enum Tag_Index
{
    tag_int,
    tag_char,
    tag_bool,
    tag_null_list
};

enum Tag_Values
{
    tv_int       = 0,
    tv_char      = 0b00001111,
    tv_bool      = 0b00011111,
    tv_null_list = 0b00101111
};

struct Tag
{
    enum Tag_Values tag;
    uint8_t mask;
    uint8_t shift;
    char type[16];
};

struct Tag tags[] =
{
    {tv_int      , 0x03, 2, "SysInt"},
    {tv_char     , 0xff, 8, "Char"},
    {tv_bool     , 0x7f, 7, "Boolean"},
    {tv_null_list, 0xff, 8, "Null List"}
};


size_t scheme_entry();

int main(int argc, char** argv)
{
    size_t val = (size_t) scheme_entry();

    if((val & tags[tag_null_list].mask) == tv_null_list)
    {
        printf("()");
    }
    else if((val & tags[tag_int].mask) == tv_int)
    {
        printf("%d", (int) val >> tags[tag_int].shift);
    }
    else if((val & tags[tag_char].mask) == tv_char)
    {
        printf("#\\%c", (char) ((val >> tags[tag_char].shift)) & 0xff);
    }
    else if((val & tags[tag_bool].mask) == tv_bool)
    {
        printf("#%c", (val >> tags[tag_bool].shift) == 1 ? 't' : 'f');
    }
    else
    {
        printf("Unknown return type: %lb", val);
    }

    printf("\n");

    return 0;
}