/*
 *	Copyright (C) 2020 PCSX2 Dev Team
 *
 *  This Program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This Program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with GNU Make; see the file COPYING.  If not, write to
 *  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA USA.
 *  http://www.gnu.org/copyleft/gpl.html
 *
 */

#ifdef __APPLE__

#if ! __has_feature(objc_arc)
#error "Compile this with -fobjc-arc"
#endif

#include "GSWndMTL.h"
#include "GSWndMTLShim.h"

std::shared_ptr<GSWnd> makeGSWndMTL()
{
	return std::make_shared<GSWndMTL>();
}

GSWndMTL::GSWndMTL() : m_NativeWindow(nil), m_view(nil), m_layer(nil)
{
}
GSWndMTL::~GSWndMTL()
{
}

void GSWndMTL::InitLayer()
{
	if (!m_NativeWindow)
		throw GSDXRecoverableError();

	dispatch_sync(dispatch_get_main_queue(), [&]{
		NSView* contentView = [m_NativeWindow contentView];
		m_view = [[NSView alloc] initWithFrame:[contentView frame]];
		m_layer = [CAMetalLayer layer];

		[m_view setWantsLayer:YES];
		[m_view setLayer:m_layer];
		[m_view setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
		// Note: Don't replace the window's contentView because this window might be from wx, in which case replacing its contentView would break wx things
		[contentView addSubview:m_view];
	});
}

bool GSWndMTL::Create(const std::string& title, int w, int h)
{
	if (m_NativeWindow)
		throw GSDXRecoverableError();

	if (w <= 0 || h <= 0)
	{
		w = theApp.GetConfigI("ModeWidth");
		h = theApp.GetConfigI("ModeHeight");
	}

	m_managed = true;

	dispatch_sync(dispatch_get_main_queue(), [&]{
		NSWindowStyleMask style = NSWindowStyleMaskTitled | NSWindowStyleMaskClosable | NSWindowStyleMaskMiniaturizable;
		m_NativeWindow = [[NSWindow alloc]
			initWithContentRect:NSMakeRect(0, 0, w, h)
			          styleMask:style
			            backing:NSBackingStoreBuffered
			              defer:NO];
	});

	InitLayer();

	return true;
}

bool GSWndMTL::Attach(void* handle, bool managed)
{
	dispatch_sync(dispatch_get_main_queue(), [&]{
		m_NativeWindow = [(__bridge NSView*)handle window];
	});
	m_managed = managed;

	InitLayer();

	return true;
}

void GSWndMTL::Detach()
{
	m_layer = nil;
	dispatch_sync(dispatch_get_main_queue(), [&]{
		[m_view removeFromSuperview];
	});
	m_view = nil;
	m_NativeWindow = nil;
}

void* GSWndMTL::GetDisplay()
{
	void *screen;
	dispatch_sync(dispatch_get_main_queue(), [&]{
		screen = (__bridge void*)[m_NativeWindow screen];
	});
	return screen;
}

GSVector4i GSWndMTL::GetClientRect()
{
	NSRect rect;
	dispatch_sync(dispatch_get_main_queue(), [&]{
		rect = [m_view convertRectToBacking:[m_view frame]];
	});
	return GSVector4i(rect.origin.x, rect.origin.y, rect.size.width, rect.size.height);
}

bool GSWndMTL::SetWindowText(const char* title)
{
	if (!m_managed)
		return true;
	dispatch_sync(dispatch_get_main_queue(), [&]{
		[m_NativeWindow setTitle:[[NSString alloc] initWithCString:title encoding:NSUTF8StringEncoding]];
	});
	return true;
}

void GSWndMTL::Show()
{
	dispatch_sync(dispatch_get_main_queue(), [&]{
		[m_NativeWindow makeKeyAndOrderFront:nil];
	});
}

void GSWndMTL::Hide()
{
	dispatch_sync(dispatch_get_main_queue(), [&]{
		[m_NativeWindow orderOut:nil];
	});
}

void GSWndMTL::HideFrame()
{
	dispatch_sync(dispatch_get_main_queue(), [&]{
		[m_NativeWindow setStyleMask:[m_NativeWindow styleMask] & ~NSWindowStyleMaskTitled];
	});
}

#endif // __APPLE__
