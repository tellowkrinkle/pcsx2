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

#include "stdafx.h"
#include "GSDeviceMTL.h"

#if ! __has_feature(objc_arc)
#error "Compile this with -fobjc-arc"
#endif

#ifdef __APPLE__

GSDeviceMTL::GSDeviceMTL()
	: m_dev(nil)
{
}
GSDeviceMTL::~GSDeviceMTL()
{
}

bool GSDeviceMTL::Create(const std::shared_ptr<GSWnd> &wnd)
{
	if (!GSDevice::Create(wnd))
		return false;

	if (m_dev)
		fprintf(stderr, "Metal: Create called but already had a device\n");
	m_dev = nil;

	std::string adapter_id = theApp.GetConfigS("Adapter");
	NSString* ns_adapter_id = [NSString
		stringWithCString:adapter_id.c_str()
		         encoding:[NSString defaultCStringEncoding]];
	for (id<MTLDevice> dev : MTLCopyAllDevices())
	{
		if ([[dev name] isEqualToString:ns_adapter_id])
		{
			m_dev = dev;
			break;
		}
	}
	if (!m_dev)
	{
		m_dev = MTLCreateSystemDefaultDevice();
		if (adapter_id != "default")
			fprintf(stderr, "Metal: Missing device %s, using default\n", adapter_id.c_str());
	}

	m_layer = (__bridge CAMetalLayer*)m_wnd->GetHandle();
	ASSERT([m_layer isKindOfClass:[CAMetalLayer class]]);
	m_layer.device = m_dev;

	// TODO: Load from resource
	m_shaders = [m_dev newDefaultLibrary];


	return true;
}

//GSTexture* CreateSurface(int type, int w, int h, int format) override;
//
//void DoMerge(GSTexture* sTex[3], GSVector4* sRect, GSTexture* dTex, GSVector4* dRect, const GSRegPMODE& PMODE, const GSRegEXTBUF& EXTBUF, const GSVector4& c) override;
//void DoInterlace(GSTexture* sTex, GSTexture* dTex, int shader, bool linear, float yoffset) override;

uint16 GSDeviceMTL::ConvertBlendEnum(uint16 generic)
{
	switch (generic)
	{
	case SRC_COLOR       : return MTLBlendFactorSourceColor;
	case INV_SRC_COLOR   : return MTLBlendFactorOneMinusSourceColor;
	case DST_COLOR       : return MTLBlendFactorDestinationColor;
	case INV_DST_COLOR   : return MTLBlendFactorOneMinusBlendColor;
	case SRC1_COLOR      : return MTLBlendFactorSource1Color;
	case INV_SRC1_COLOR  : return MTLBlendFactorOneMinusSource1Color;
	case SRC_ALPHA       : return MTLBlendFactorSourceAlpha;
	case INV_SRC_ALPHA   : return MTLBlendFactorOneMinusSourceAlpha;
	case DST_ALPHA       : return MTLBlendFactorDestinationAlpha;
	case INV_DST_ALPHA   : return MTLBlendFactorOneMinusDestinationAlpha;
	case SRC1_ALPHA      : return MTLBlendFactorSource1Alpha;
	case INV_SRC1_ALPHA  : return MTLBlendFactorOneMinusSource1Alpha;
	case CONST_COLOR     : return MTLBlendFactorBlendColor;
	case INV_CONST_COLOR : return MTLBlendFactorOneMinusBlendColor;
	case CONST_ONE       : return MTLBlendFactorOne;
	case CONST_ZERO      : return MTLBlendFactorZero;
	case OP_ADD          : return MTLBlendOperationAdd;
	case OP_SUBTRACT     : return MTLBlendOperationSubtract;
	case OP_REV_SUBTRACT : return MTLBlendOperationReverseSubtract;
	default              : ASSERT(0); return 0;
	}
}

//bool Create(const std::shared_ptr<GSWnd> &wnd) override;
//bool Reset(int w, int h) override;
//bool IsLost(bool update = false) override;
//void Present(const GSVector4i& r, int shader) override;
//void Present(GSTexture* sTex, GSTexture* dTex, const GSVector4& dRect, ShaderConvert shader = ShaderConvert::COPY) override;
//void Flip() override;
//
//void SetVSync(int vsync) override;
//
//void BeginScene() override;
//void DrawPrimitive() override;
//void DrawIndexedPrimitive() override;
//void DrawIndexedPrimitive(int offset, int count) override;
//void EndScene() override;
//
//bool HasDepthSparse() override;
//bool HasColorSparse() override;
//
//void ClearRenderTarget(GSTexture* t, const GSVector4& c) override;
//void ClearRenderTarget(GSTexture* t, uint32 c) override;
//void ClearDepth(GSTexture* t) override;
//void ClearStencil(GSTexture* t, uint8 c) override;
//
//GSTexture* CopyOffscreen(GSTexture* src, const GSVector4& sRect, int w, int h, int format = 0, ShaderConvert ps_shader = ShaderConvert::COPY) override;
//
//void CopyRect(GSTexture* sTex, GSTexture* dTex, const GSVector4i& r) override;
//void StretchRect(GSTexture* sTex, const GSVector4& sRect, GSTexture* dTex, const GSVector4& dRect, ShaderConvert shader = ShaderConvert::COPY, bool linear = true) override;
//void StretchRect(GSTexture* sTex, const GSVector4& sRect, GSTexture* dTex, const GSVector4& dRect, bool red, bool green, bool blue, bool alpha) override;
//
//void StretchRect(GSTexture* sTex, GSTexture* dTex, const GSVector4& dRect, int shader = 0, bool linear = true);
//
//void PSSetShaderResources(GSTexture* sr0, GSTexture* sr1) override;
//void PSSetShaderResource(int i, GSTexture* sRect) override;
//void OMSetRenderTargets(GSTexture* rt, GSTexture* ds, const GSVector4i* scissor = NULL) override;

#endif
