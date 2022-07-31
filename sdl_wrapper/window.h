#ifndef WINDOW_H_INCLUDED
#define WINDOW_H_INCLUDED
#include <sdl2/sdl.h>
#include "bitarray.h"
#include <cassert>

extern "C"
{
    struct cell_coordinates
    {
        cell_coordinates(Uint8 ix,Uint8 iy) : x{ix},y{iy} {}
        Uint8 x,y;
    };
    struct color
    {
        Uint8 red, green, blue;
    };
    struct gramma
    {
        Uint8 code, red, green, blue; //To grant compactness
    };
    struct rectangle
    {
        cell_coordinates start,size;
    };
}

cell_coordinates operator+(cell_coordinates,cell_coordinates);
cell_coordinates operator-(cell_coordinates,cell_coordinates);
gramma make_gramma(const Uint8,const color);

class SDLRender
{
public:
    SDLRender(SDL_Window* win) : renderer_ {renderer_create(win)} {}

    //Drawing
    void draw_pixel(SDL_Point);
    void set_draw_color(SDL_Color);
    void fill_rectangle(SDL_Rect,SDL_Color);

    //Presenting
    void present(void) {SDL_RenderPresent(renderer_);}
    void clear();

    ~SDLRender(void) {SDL_DestroyRenderer(renderer_);}
private:
    SDL_Renderer* renderer_create(SDL_Window*);
    SDL_Renderer* renderer_;
};

class SDLWindow : private SDLRender
{
public:

//Drawing
    void draw_gramma(const gramma,const cell_coordinates);
    void fill_cell(const cell_coordinates,const color);
    void fill_rectangle(const rectangle,const color);
    void draw_frame(const rectangle,const color);

//Rendering
    using SDLRender::present,SDLRender::clear;

//Access
    static SDLWindow& instance(void) {return *instance_;}

//Initializatin
    static void initialize(void)//Call it after a call to SDL_InitSystems
    {
        #ifndef NDEBUG
        static bool was_init = false;
        #endif
        assert(!was_init);

        was_init = true;

        instance_ = new SDLWindow(win_create());
        instance_->init_drawer();
    }

    //Constants
    static const unsigned width_=80, height_=25;
    static constexpr unsigned cell_width = 8, cell_height = 12;
private:
    SDLWindow(SDL_Window* win) : window_{win},SDLRender{win} {}
    ~SDLWindow(void) {SDL_DestroyWindow(window_);}

    SDL_Window* window_;
//    SDLRender render_;

    using compressed_pixels = bit_array<cell_height*cell_width>;

    compressed_pixels bitmap_font[256];

    static SDL_Window* win_create(void);
    void init_drawer();

    static SDLWindow* instance_;

};

#endif // WINDOW_H_INCLUDED
