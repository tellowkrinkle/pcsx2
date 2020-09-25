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

#pragma once

#include "GSWnd.h"

#ifndef __OBJC__
#error "This header is for use with Objective-C++ only.  You probably wanted GSWndMTLShim.h"
#endif

#ifdef __APPLE__

#include <AppKit/AppKit.h>
#include <QuartzCore/QuartzCore.h>

class GSWndMTL final : public GSWnd
{
	NSWindow* m_NativeWindow;
	NSView* m_view;
	CAMetalLayer* m_layer;

	void InitLayer();

public:
	GSWndMTL();
	virtual ~GSWndMTL();

	bool Create(const std::string& title, int w, int h) override;
	bool Attach(void* handle, bool managed = true) override;
	void Detach() override;

	void* GetDisplay() override;
	void* GetHandle() override { return (__bridge void*)(m_layer); };
	GSVector4i GetClientRect() override;
	bool SetWindowText(const char* title) override;

	void Show() override;
	void Hide() override;
	void HideFrame() override;
};

#endif
