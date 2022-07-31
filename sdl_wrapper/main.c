#include "main.h"
#include <SDL2/SDL.h>
#include "window.h"
#include <windows.h>
#define DLL_EXPORT __declspec(dllexport)

void destroy_systems(void)
{

}

extern "C"
{
    DLL_EXPORT void draw_gramma(gramma* gr,cell_coordinates* pos)
    {
        SDLWindow::instance().draw_gramma(*gr,*pos);
    }

    DLL_EXPORT void fill_cell(color* clr,cell_coordinates* pos)
    {
        SDLWindow::instance().fill_cell(*pos,*clr);
    }

    DLL_EXPORT void draw_rectangle(rectangle* rect,color* clr)
    {
        SDLWindow::instance().fill_rectangle(*rect,*clr);
    }
    DLL_EXPORT void draw_frame(rectangle* rect,color* clr)
    {
        SDLWindow::instance().draw_frame(*rect,*clr);
    }
    DLL_EXPORT void clear(void)
    {
        SDLWindow::instance().clear();
    }
    DLL_EXPORT void present(void)
    {
        SDLWindow::instance().present();
    }

    DLL_EXPORT SDL_Scancode key_to_event(SDL_Keycode code)
    {
        return SDL_GetScancodeFromKey(code);
    }

    DLL_EXPORT int get_key_event(void)
    {
        SDL_Event event;
        do {
            if(!SDL_PollEvent(&event))
                SDL_WaitEvent(&event);
        } while(event.type != SDL_KEYDOWN);

        int ret = event.key.keysym.scancode;
        if(ret <= SDL_SCANCODE_KP_0 && ret >= SDL_SCANCODE_KP_1)
            ret = ret - ((int)SDL_SCANCODE_KP_0 - (int)SDL_SCANCODE_0);

        return ret;
    }


    DLL_EXPORT void initialize(void)
    {
    SDL_Init(SDL_INIT_VIDEO|SDL_INIT_EVENTS);
    SDLWindow::instance().initialize();

    SDLWindow::instance().clear();

    const char str[] = "Initialization done.";
    for(size_t i = 0;str[i];++i){
        SDLWindow::instance().draw_gramma({static_cast<Uint8>(str[i]),(Uint8)100,(Uint8)100,(Uint8)100},{i,0});
    }

    /*SDL_Event ev;
    do {
        SDLWindow::instance().present();
        SDL_WaitEvent(&ev);
    }while(ev.type != SDL_KEYDOWN);
    */
    }
}

extern "C"
{
DLL_EXPORT BOOL APIENTRY DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
    switch (fdwReason)
    {
        case DLL_PROCESS_ATTACH:
            // attach to process
            // return FALSE to fail DLL load
            DisableThreadLibraryCalls( hinstDLL );
            break;

        case DLL_PROCESS_DETACH:
            // detach from process
            break;

        case DLL_THREAD_ATTACH:
            // attach to thread
            break;

        case DLL_THREAD_DETACH:
            // detach from thread
            break;
    }
    return TRUE; // succesful
}
}
